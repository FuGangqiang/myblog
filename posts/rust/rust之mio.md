date: 2017-01-18 20:55:38
tags: rust, tokio, network


随着 tokio 0.1 的发布，rust 语言的异步 io 库逐渐成熟，
打算用几篇博客来描述一下 tokio 框架相关的基础 crate(当然，也是为了学习一下)：

* [mio][]
* [futures-rs][]
* [tokio-core][]
* [tokio-proto][]
* [tokio-service][]

[mio]: https://github.com/carllerche/mio
[futures-rs]: https://github.com/alexcrichton/futures-rs
[tokio-core]: https://github.com/tokio-rs/tokio-core
[tokio-proto]: https://github.com/tokio-rs/tokio-proto
[tokio-service]: https://github.com/tokio-rs/tokio-service

本篇主要讲解 [mio][]。

mio(Metal IO) 是 rust 语言中的异步 io crate，tokio 框架就是基于这个 crate 的。
从源码上看 mio 基本就是对 linux epoll(bsd kqueue) 的简单包装，只是披了一层 rust 外衣，
性能与原生相同，接口调用方式与 linux epoll 极为相似，mio 本身实现了可以监控以下几种事件：

* tcp io
* udp io
* timer
* channel(mpsc)

同时，mio 也提供了一种扩展机制，可以很轻松的实现以下类似事件：

* uds(unix domain socket)
* signal
* pipe
* thread
* subprocess

当然在看这篇文章之前，你需要了解 linux epoll 的知识，
可以查看博文[如何使用epoll系统调用](/blog/posts/linux/如何使用epoll系统调用.html)，本文示例与 epoll 示例功能实现完全相同。

我们可以通过以下示例来看一下 mio 的用法：

```rust
extern crate mio;
extern crate slab;

use std::net::SocketAddr;
use std::io::{Read, Write, ErrorKind};
use mio::*;
use mio::tcp::TcpListener;

type Slab<T> = slab::Slab<T, Token>;

const SERVER_TOKEN: Token = Token(::std::usize::MAX-1);


fn main() {
    let mut args = ::std::env::args();
    let cmd = args.next().unwrap();
    let port = args.next().expect(&format!("Usage: {} [port]", cmd));
    let addr: SocketAddr = format!("127.0.0.1:{}", port).parse().expect("argument format error: port");
    let server = TcpListener::bind(&addr).expect("socket binding error");
    let poll = Poll::new().expect("poll create error");
    let mut conns = Slab::with_capacity(1024);
    let mut events = Events::with_capacity(1024);
    let mut buf: [u8; 1024] = [0; 1024];
    let stdout = ::std::io::stdout();

    poll.register(&server, SERVER_TOKEN, Ready::readable(), PollOpt::edge())
        .expect("poll register error");
    // the event loop
    loop {
        poll.poll(&mut events, None).expect("poll error");
        for event in events.iter() {
            let (token, kind) = (event.token(), event.kind());

            if kind.is_error() || kind.is_hup() || !kind.is_readable() {
                println!("kind error");
                if token == SERVER_TOKEN {
                    ::std::process::exit(1);
                }
                conns.remove(token);
            } else if token == SERVER_TOKEN {
                loop {
                    let sock = match server.accept() {
                        Ok((sock, addr)) => {
                            println!("Accepted connection: {}", addr);
                            sock
                        }
                        Err(_) => break
                    };
                    let new_token = conns.insert(sock).expect("add connection error");
                    poll.register(&conns[new_token], new_token, Ready::readable(), PollOpt::edge())
                        .expect("poll register error");

                }
            } else {
                let mut need_to_close = false;
                {
                    let ref mut client = conns[token];
                    loop {
                        match client.read(&mut buf) {
                            Ok(n) => {
                                if n == 0 {
                                    need_to_close = true;
                                    break;
                                } else {
                                    let mut handle = stdout.lock();
                                    handle.write(&buf[..n]).expect("write error");
                                    handle.flush().expect("flush error");
                                }
                            },
                            Err(e) => {
                                if e.kind() != ErrorKind::WouldBlock {
                                    need_to_close = true;
                                }
                                break;
                            }
                        }
                    }
                }
                if need_to_close {
                    println!("Closing connection on token={:?}", token);
                    conns.remove(token);
                }

            }
        }
    }
}
```

我们从源码上面看，代码结构与 linux epoll 大致一样，
有一个显著的不同就是 mio 用了 token(usize 类型) 来绑定 tcp 链接，
这个应该主要是因为在 linux 系统中，每一个 socket 都是一个文件，对应一个文件描述符(usize 类型)，
而 rust 语言标准库里并没有跨平台的文件描述符类型，
而且也为了扩展其他事件类型，
引入 token 机制，统一了事件的索引类型。

示例代码中也引入了 `slab` crate 用来保存 token 和链接的一一对应关系，
当然，你也可以用 hashmap 来建立索引，不过这个不是很高效。

示例代码 repo 可以到[mio-example](https://github.com/FuGangqiang/example/tree/master/mio)下载。
