created: 2018-04-11T15:10:40+08:00
tags: [rust, tokio, network]


随着 tokio 0.1 的发布，rust 语言的异步 io 库逐渐成熟，
打算用几篇博客来描述一下 tokio 框架相关的基础 crate(当然，也是为了学习一下)：

* [mio][]
* [futures][]
* [tokio][]

[mio]: https://github.com/carllerche/mio
[futures]: https://github.com/alexcrichton/futures-rs
[tokio]: https://github.com/tokio-rs/tokio

本篇主要讲解 [tokio][]。

直接通过 mio 我们也是可以写出异步 IO 程序的，但是 mio 提供的接口太过于底层，
以至于直接用 mio 时需要保存 token 和链接的关系，并且还要在链接就绪后去执行特定的程序，
这些都太过于繁杂，但是这个过程存在一个事件分派模式，
我们可以把这个模式抽象出来，所以 tokio 应运而生。

tokio 基于 mio 和 futures，
通过 mio 来管理 IO 事件，通过 futures 来表达对各种 IO 事件的处理，
这样 tokio 提供了更方便的接口供用户使用。

我们可以通过以下代码示例来说明 tokio 的用法：

```rust
extern crate tokio;

use tokio::io;
use tokio::net::TcpListener;
use tokio::prelude::*;

fn main() {
    let addr = "127.0.0.1:12345".parse().unwrap();
    let tcp = TcpListener::bind(&addr).unwrap();

    let server = tcp.incoming().for_each(|tcp| {
        let (reader, writer) = tcp.split();
        let conn = io::copy(reader, writer)
            .map(|(n, _, _)| {
                println!("wrote {} bytes", n)
            })
            .map_err(|err| {
                println!("IO error {:?}", err)
            });

        tokio::spawn(conn);
        Ok(())
    })
    .map_err(|err| {
        println!("server error {:?}", err);
    });

    tokio::run(server);
}
```

上面代码实现的功能与 epoll/mio 篇中实现的功能完全一样，通过对比，
我们可以看出 tokio 写出的代码更具有可读性，
它隐藏了直接通过 mio 接口调用的大量繁杂的代码，
又通过各种自定义的 futures 来定义事件处理程序。
