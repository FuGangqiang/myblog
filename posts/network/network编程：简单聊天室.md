created: 2015-04-27T15:42:00+08:00
tags: [c, network]


这里是一个用 C 语言写的一个简单聊天室 server 版本，
当运行此程序后，就可以用多个 telnet 终端连接 server 测试了。


## 代码

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>


#define PORT "9034"

void *get_in_addr(struct sockaddr *sa)
{
  if (sa->sa_family == AF_INET) {
    return &(((struct sockaddr_in*)sa)->sin_addr);
  }
  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

int main(void)
{
  fd_set master;
  fd_set read_fds;
  int fdmax;

  int listener;
  int newfd;
  struct sockaddr_storage remoteaddr;
  socklen_t addrlen;

  char buf[256];
  int nbytes;

  char remoteIP[INET6_ADDRSTRLEN];

  int yes=1;
  int i, j, rv;

  struct addrinfo hints, *ai, *p;

  FD_ZERO(&master);
  FD_ZERO(&read_fds);

  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  if ((rv = getaddrinfo(NULL, PORT, &hints, &ai)) != 0) {
    fprintf(stderr, "selectserver: %s\n", gai_strerror(rv));
    exit(1);
  }

  for(p = ai; p != NULL; p = p->ai_next) {
    listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (listener < 0) {
      continue;
    }

    setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

    if (bind(listener, p->ai_addr, p->ai_addrlen) < 0) {
      close(listener);
      continue;
    }
    break;
  }

  if (p == NULL) {
    fprintf(stderr, "selectserver: failed to bind\n");
    exit(2);
  }

  freeaddrinfo(ai);

  if (listen(listener, 10) == -1) {
    perror("listen");
    exit(3);
  }

  FD_SET(listener, &master);

  fdmax = listener;

  for (;;) {
    read_fds = master;
    if (select(fdmax+1, &read_fds, NULL, NULL, NULL) == -1) {
      perror("select");
      exit(4);
    }

    for (i = 0; i <= fdmax; i++) {
      if (!FD_ISSET(i, &read_fds)) {
        continue;
      }
      if (i==listener) {
        addrlen = sizeof remoteaddr;
        newfd = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);

        if (newfd == -1) {
          perror("accept");
        } else {
          FD_SET(newfd, &master);
          if (newfd > fdmax) {
            fdmax = newfd;
          }
          printf("selectserver: new connection from %s on socket %d\n",
                 inet_ntop(remoteaddr.ss_family,
                           get_in_addr((struct sockaddr*)&remoteaddr),
                           remoteIP,
                           INET6_ADDRSTRLEN),
                 newfd);
        }
        continue;
      }

      if ((nbytes = recv(i, buf, sizeof buf, 0)) <= 0) {
        if (nbytes == 0) {
          printf("selectserver: socket %d hung up\n", i);
        } else {
          perror("recv");
        }
        close(i);
        FD_CLR(i, &master);
        continue;
      }
      for (j = 0; j <= fdmax; j++) {
        if (!FD_ISSET(j, &master)) {
          continue;
        }
        if (j == listener || j == i) {
          continue;
        }
        if (send(j, buf, nbytes, 0) == -1) {
          perror("send");
        }
      }
    }
  }

  return 0;
}
```
