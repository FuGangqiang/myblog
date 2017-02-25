date: 2017-01-19 21:58:59
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

本篇主要讲解 [futures-rs][]。

在许多编程语言都有 `futures`(类似于 `promise`、`delay`、`deferred`) 的概念，
并在此基础上引入轻量级进程或协程的概念。


## 为什么需要 futures？

在异步并发编程中同时需要维护成千上万个链接，这些链接大多处于阻塞状态，
当其中某些链接就绪后（由 mio 提供的接口询问），服务程序需要分别对其进行不同的处理，
处理这些链接的方法大多用两种方式：

* callbacks
* futures

虽然 callbacks 方式便于理解，但是当回调的嵌套层次比较深时，程序代码维护和阅读很不方便，
为此引入了 futures 方式来表达程序的执行流程，进而使代码像同步代码一样。


## 什么是 futures？

本质上 `future` 就是一个值，但是这个值有可能还没有就绪，而就绪是需要某些事件的发生，
其适用于任何事件，我们可以用 `future` 来表示以下事件：

* 一个 RPC 调用
* 一个数据库查询
* 一个计时器
* 一个非常耗时的任务
* 从 socket 中读数据

不仅如此，我们可以对 `future` 进行任意的组合，比如：

* 顺序组合(sequential composition)：
  `f.and_then(|val| some_new_future(val))`，
  获取 future `f` 的值，将其传入一个函数进而构造另一个 future
* 映射组合(Mapping)：
  `f.map(|val| some_new_value(val))`，
  获取 future `f` 的值，将其传入一个函数进而构造另一个值
* 合并组合(Joining)：
  `f.join(g)`，
  使 future `f` 和 future `g` 并发执行，当两者都就绪后将两者的值合并为一个元组
* 选择组合(Selecting)：
  `f.select(g)`，
  使 future `f` 和 future `g` 并发执行，当其中一个就绪后并选择这个 future 的值返回

通过上面的特性我们可以很容易用 `futures` 来表达下面的执行流：

```rust
id_rpc(&my_server).and_then(|id| {
    get_row(id)
}).map(|row| {
    json::encode(row)
}).and_then(|encoded| {
    write_string(my_socket, encoded)
})
```

这看起来很像同步代码，但是上面代码最终会被编译成一个状态机，根据当前的状态执行特定程序。


## futures-rs future 种类

从上面看 future 只是代表一个单值，只代表一个事件，
但是在网络编程中，读取一个 TCP 链接我们通常会获取一个字节流，与一系列读事件相关，
因此我们还需要另一种 future 抽象用来关联多个事件：

* Stream：读入流，与 incoming events 关联
* Sink：写出流，与 outgoing events 关联

因此 futures-rs 分别提供了三种 trait：

* Future
* Stream
* Sink


## trait 定义

trait `Future` 的定义与标准库中的 `Iterator` 很相似：

```rust
trait Future {
    // The type of value that the future yields on successful completion.
    type Item;

    // The type of value that the future yields on failure.
    type Error;

    // The only required method, which attempts to complete the future.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error>;

	...
}

```

定义一个 `Future` 只需定义 `Item`、`Error` 类型和 `poll` 方法就可以了，
其他与 `Future` 有关的方法都是基于这三个的，已经预先定义了，不需要用户自定义了。

`Stream` 和 `Sink` 也与 `Future` 类似：

```rust
trait Stream {
    // The type of item yielded each time the stream's event occurs
    type Item;

    // The error type; errors terminate the stream.
    type Error;

    // Try to produce a value.
    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error>;

    ...
}

trait Sink {
    // The type of value that the sink accepts.
    type SinkItem;

    // The type of value produced by the sink when an error occurs.
    type SinkError;

    // The analog to `poll`, used for sending and then flushing items.
    fn start_send(&mut self, item: Self::SinkItem) -> StartSend<Self::SinkItem, Self::SinkError>;

    fn poll_complete(&mut self) -> Poll<(), Self::SinkError>;

    ...
}
```
