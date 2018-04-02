created: 2017-01-20T16:07:40+08:00
tags: [rust, tokio, network]


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

本篇主要讲解 [tokio-core][]。

直接通过 mio 我们也是可以写出异步 IO 程序的，但是 mio 提供的接口太过于底层，
以至于直接用 mio 时需要保存 token 和链接的关系，并且还要在链接就绪后去执行特定的程序，
这些都太过于繁杂，但是这个过程存在一个事件分派模式，
我们可以把这个模式抽象出来，所以 tokio-core 应运而生。

tokio-core 基于 mio 和 futures-rs，
通过 mio 来管理 IO 事件，通过 futures-rs 来表达对各种 IO 事件的处理，
这样 tokio-core 提供了更方便的接口供用户使用。

我们可以通过以下代码示例来说明 tokio-core 的用法：

```rust
extern crate futures;
extern crate tokio_core;

use std::net::SocketAddr;

use futures::Future;
use futures::stream::Stream;
use tokio_core::io::{copy, Io};
use tokio_core::net::TcpListener;
use tokio_core::reactor::Core;


fn main() {
    let mut args = ::std::env::args();
    let cmd = args.next().unwrap();
    let port = args.next().expect(&format!("Usage: {} [port]", cmd));
    let addr: SocketAddr = format!("127.0.0.1:{}", port).parse().expect("argument format error: port");

    let mut core = Core::new().unwrap();  // the Event Loop
    let handle = core.handle();

    let socket = TcpListener::bind(&addr, &handle).unwrap();
    let done = socket.incoming().for_each(|(socket, addr)| {
        let pair = futures::lazy(|| Ok(socket.split()));
        let amt = pair.and_then(|(reader, _)| copy(reader, ::std::io::stdout()));
        println!("Accepted connection: {}", addr);
        handle.spawn(amt.then(move |_| {
            println!("Closing connection on {}", addr);
            Ok(())
        }));
        Ok(())
    });

    // Start Event Loop
    core.run(done).unwrap();
}
```

上面代码实现的功能与 epoll/mio 篇中实现的功能完全一样，通过对比，
我们可以看出 tokio-core 写出的代码更具有可读性，
它隐藏了直接通过 mio 接口调用的大量繁杂的代码，
又通过各种自定义的 futures 来定义事件处理程序。

上面程序需要注意的一点是：
`tokio_core::net` 中提供的所有 udp/tcp 相关的方法都已通过 futures-rs 进行了包装，
这样就可以通过 futures 之间的组合方法来编写各种各样链接的处理方法了。

示例代码 repo 可以到 [tokio-core-example](https://github.com/FuGangqiang/example/tree/master/tokio-core) 下载。
