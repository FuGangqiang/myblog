date: 2017-01-22 20:29:30
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

本篇是这一系列的最后一篇，涉及到 tokio 框架的 Trait `CodeC`、`ServerProto`、`Service`。


## 网络应用结构

我们知道，大多数网路应用的结构都被设计成一个分层结构：

* 字节流层(byte streams)
* 分帧层(framing)
* 请求/响应交换层(request / response exchange)
* 应用层(application)

这样的设计可以使应用程序结构化、模块化，使各层程序只需关注自己所关注的事情，
便于维护，更具可读性。


### 字节流层(byte streams)

这一层是最底一层，数据直接由 TCP/UDP socket 提供的数据，
在这一层进行的所有操作大多通过缓冲区来操作字节数组(byte arrays)。


### 分帧层(framing)

字节流通常被分割成数个有意义的单位，每个单位被称为一个数据帧(frame)，
比如：
HTTP 协议包含请求头(request headers)、响应头(response headers)和包体块(body chunks)。
一种基于行的协议由换行符`\n`分割。
抽象出分帧层，我们就不必直接操作裸字节流了，以后所有操作可以基于数据帧了。


### 请求/响应交换层(request / response exchange)

在 server/client 应用中，通常客户端发起一个 request，服务端就会返回一个 reponse，
每个 request 有可能有多个数据帧组成，
这样我们需要维护 request/response/transport 之间的对应关系。


### 应用层(application)

在这一层，我们并不关心 request/response 和 transport 的对应关系，
只能看到请求对象，然后根据应用逻辑产生对应的相应对象。


### 相关 Trait

每一层虽然进行的操作各有不同，但在相邻层间的转换存在一个模式，可以把它们抽象出来，
因此有了：

* `tokio_core::io::Codec`
* `tokio_proto::pipeline::ServerProto` 和 `tokio_proto::multiplex::ServerProto`
* `tokio_service::Service`


## 示例

最后我们通过经典的 `echo server` 示例代码来看一下这几个 Trait 的用法：

```rust
extern crate byteorder;
extern crate futures;
extern crate tokio_core;
extern crate tokio_proto;
extern crate tokio_service;

use std::io;
use std::str;
use std::net::SocketAddr;

use byteorder::{BigEndian, ByteOrder};
use futures::Future;
use futures::future::{self, BoxFuture};
use tokio_core::io::{Io, Framed, EasyBuf, Codec};
use tokio_proto::TcpServer;
use tokio_proto::multiplex::{RequestId, ServerProto};
use tokio_service::Service;

struct LineCodec;
struct LineProto;
struct EchoService;


/// # Frame Struct:
///
/// * request id: 4 byte
/// * payload: zero or more bytes
/// * linefeed: `\n`
///
/// +-- request id --+------- frame payload --------+
/// |                |                              |
/// |   \x00000001   | This is the frame payload \n |
/// |                |                              |
/// +----------------+------------------------------+
///
impl Codec for LineCodec {
    type In = (RequestId, String);
    type Out = (RequestId, String);

    fn decode(&mut self, buf: &mut EasyBuf) -> Result<Option<(RequestId, String)>, io::Error>
    {
        if buf.len() < 5 {
            return Ok(None);  // We don't yet have a full message
        }

        let newline = buf.as_ref()[4..].iter().position(|b| *b == b'\n');
        if let Some(n) = newline {
            let line = buf.drain_to(n + 4);
            buf.drain_to(1);
            let id = BigEndian::read_u32(&line.as_ref()[0..4]);
            return match str::from_utf8(&line.as_ref()[4..]) {
                Ok(s) => Ok(Some((id as RequestId, s.to_string()))),
                Err(_) => Err(io::Error::new(io::ErrorKind::Other, "invalid string")),
            }
        }

        Ok(None)  // We don't yet have a full message
    }

    fn encode(&mut self, msg: (RequestId, String), buf: &mut Vec<u8>) -> io::Result<()>
    {
        let (id, msg) = msg;
        let mut encoded_id = [0; 4];

        BigEndian::write_u32(&mut encoded_id, id as u32);
        buf.extend(&encoded_id);
        buf.extend(msg.as_bytes());
        buf.push(b'\n');

        Ok(())
    }
}


impl<T: Io + 'static> ServerProto<T> for LineProto {
    type Request = String;
    type Response = String;

    // `Framed<T, LineCodec>` is the return value of `io.framed(LineCodec)`
    type Transport = Framed<T, LineCodec>;
    type BindTransport = Result<Self::Transport, io::Error>;

    fn bind_transport(&self, io: T) -> Self::BindTransport {
        Ok(io.framed(LineCodec))
    }
}


impl Service for EchoService {
    type Request = String;
    type Response = String;
    type Error = io::Error;
    type Future = BoxFuture<Self::Response, Self::Error>;

    fn call(&self, req: Self::Request) -> Self::Future {
        future::ok(req).boxed()
    }
}


fn main() {
    let mut args = ::std::env::args();
    let cmd = args.next().unwrap();
    let port = args.next().expect(&format!("Usage: {} [port]", cmd));
    let addr: SocketAddr = format!("127.0.0.1:{}", port).parse().expect("argument format error: port");
    let server = TcpServer::new(LineProto, addr);
    server.serve(|| Ok(EchoService));
}
```

示例代码 repo 可以到 [tokio-echo](https://github.com/FuGangqiang/example/tree/master/tokio-echo) 下载。
