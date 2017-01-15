date: 2017-01-14 23:00:43
tags: linux, network


传统的网络服务器接收链接时通常 fork 一个进程或者 create 一个线程来处理这个链接，比如 Apache 服务器，
这种方式实现起来比较简单，但是当并发链接比较大时，
一个链接一个进程或线程这种方式将会极大的消耗系统资源，进程或线程的上下文切换耗时也比较多，
进而导致性能极度低下，
为了避免这种情况，可以使用异步 I/O，
在 linux 系统中，性能最好的当属 `epoll`，功能类似于 `select` 和 `poll`，但是

* `select`：只能监控 `FD_SETSIZE` 个链接，libc 里这个数量设置的很小
* `poll`：尽管没有链接数的限制，也就是说可以监控 `RLIMIT_NOFILE` 个链接，
  但是每一次都要对所有监控链接从头到尾的扫描，速度O(n)，这也降低了性能
* 在这两个系统调用使用时，内核与用户空间通过内存复制来进行消息传递，进一步降低性能

而 `epoll` 没有 `select` 和 `poll` 的限制，
可以 O(1) 时间处理操作，
内核与用户空间通过 `mmap` 来进行消息传递，又进一步加快了速度。

与 `epoll` 相关的系统调用有：

* `epoll_create`：创建 epoll instance
* `epoll_create1`：同上
* `epoll_ctl`：用来在 epoll instance 上添加、删除被监控的文件描述符(`file descriptor`)
* `epoll_wait`：获取有事件发生的文件描述符，当没有时会一直被阻塞

当文件描述符被加入后，事件通知的方式有两种：

* `level triggered`：当你未及时处理时，一个事件会持续通知，直到你处理完毕
* `edge triggered`：一个事件只通知一次


下面我们通过一个 TCP 服务器的示例来演示 epoll 的用法，
这个服务器将所有链接发送给它的数据输出到标准输出。


我们通过函数 `create_and_bind` 来创建服务端的 TCP socket：

```c
static int
create_and_bind(char *port)
{
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int s, sfd;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;     /* Return IPv4 and IPv6 choices */
  hints.ai_socktype = SOCK_STREAM; /* We want a TCP socket */
  hints.ai_flags = AI_PASSIVE;     /* All interfaces */

  s = getaddrinfo(NULL, port, &hints, &result);
  if(s != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
    return -1;
  }

  for(rp = result; rp != NULL; rp = rp->ai_next) {
    sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if(sfd == -1)
      continue;

    s = bind(sfd, rp->ai_addr, rp->ai_addrlen);
    if(s == 0) /* We managed to bind successfully! */
      break;

    close(sfd);
  }

  if(rp == NULL) {
    fprintf(stderr, "Could not bind\n");
    return -1;
  }

  freeaddrinfo(result);
  return sfd;
}
```


通过 `make_socket_non_blocking` 函数来使 socket 非阻塞：

```c
static int
make_socket_non_blocking(int sfd)
{
  int flags, s;

  flags = fcntl(sfd, F_GETFL, 0);
  if(flags == -1) {
    perror("fcntl");
    return -1;
  }

  flags |= O_NONBLOCK;
  s = fcntl(sfd, F_SETFL, flags);
  if(s == -1) {
    perror("fcntl");
    return -1;
  }

  return 0;
}
```

通过 `register_socket` 向 epoll instance 中添加要监控的文件描述符：

```c
static void
register_socket(int efd, int sfd) {
  int s;
  struct epoll_event event;

  event.data.fd = sfd;
  event.events = EPOLLIN | EPOLLET;
  s = epoll_ctl(efd, EPOLL_CTL_ADD, sfd, &event);
  if(s == -1) {
    perror("epoll_ctl");
    abort();
  }
}
```

`Event Loop` 存在于 `main` 函数里面：

```c
int
main(int argc, char *argv[])
{
  int sfd, efd;
  struct epoll_event event;
  struct epoll_event *events;

  if(argc != 2) {
    fprintf(stderr, "Usage: %s [port]\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  efd = epoll_create1(0);
  if(efd == -1) {
    perror("epoll_create");
    abort();
  }
  events = calloc(MAXEVENTS, sizeof event);
  sfd = init_listen_socket(efd, argv[1]);

  /* The Event Loop */
  while(1) {
    int n, i;

    n = epoll_wait(efd, events, MAXEVENTS, -1);
    for(i = 0; i < n; i++) {
      if((events[i].events & EPOLLERR) || (events[i].events & EPOLLHUP) || (!(events[i].events & EPOLLIN))) {
        /* An error has occured on this fd, or the socket is not ready for reading(why were we notified then?) */
        fprintf(stderr, "epoll error\n");
        close(events[i].data.fd);
        continue;
      } else if(sfd == events[i].data.fd) {
        handle_listen_socket_event(efd, &events[i]);
        continue;
      } else {
        handle_connect_socket_event(&events[i]);
      }
    }
  }

  free(events);
  close(sfd);
  return EXIT_SUCCESS;
}
```

从上面可以看出，`Event Loop` 分别处理 `listen_socket` 和 `connect_socket`:

```c
static void
handle_listen_socket_event(int efd, struct epoll_event *event) {
  int s;

  while(1) {
    struct sockaddr in_addr;
    socklen_t in_len;
    int infd;
    char hbuf[NI_MAXHOST], sbuf[NI_MAXSERV];

    in_len = sizeof in_addr;
    infd = accept(event->data.fd, &in_addr, &in_len);
    if(infd == -1) {
      if((errno == EAGAIN) || (errno == EWOULDBLOCK)) {
        /* We have processed all incoming connections. */
        break;
      } else {
        perror("accept");
        break;
      }
    }

    s = getnameinfo(&in_addr, in_len,
                    hbuf, sizeof hbuf,
                    sbuf, sizeof sbuf,
                    NI_NUMERICHOST | NI_NUMERICSERV);
    if(s == 0)
      printf("Accepted connection on descriptor %d (host=%s, port=%s)\n", infd, hbuf, sbuf);

    s = make_socket_non_blocking(infd);
    if(s == -1)
      abort();

    register_socket(efd, infd);
  }
}


static void
handle_connect_socket_event(struct epoll_event *event) {
  int s, done = 0;

  while(1) {
    ssize_t count;
    char buf[512];

    count = read(event->data.fd, buf, sizeof buf);
    if(count == -1) {
      /* If errno == EAGAIN, that means we have read all data. */
      if(errno != EAGAIN) {
        perror("read");
        done = 1;
      }
      break;
    } else if(count == 0) {
      /* End of file. The remote has closed the connection. */
      done = 1;
      break;
    }

    /* Write the buffer to standard output */
    s = write(STDIN_FILENO, buf, count);
    if(s == -1) {
      perror("write");
      abort();
    }
  }

  if(done) {
    printf("Closed connection on descriptor %d\n", event->data.fd);
    close(event->data.fd);
  }
}
```


示例程序完整版请见[epoll example](https://github.com/FuGangqiang/epoll_example/blob/master/example.c)。
