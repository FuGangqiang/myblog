created: 2018-04-29T17:40:51+08:00
tags: [rust, concurrency]

最近重温了一下 rust 并发编程方面的知识，感觉很有必要总结一下，写个博客系列，
就从线程的创建开始吧。


## 创建线程

每一个程序默认都会有一个主线程的，也就是运行 `main` 函数的线程，
我们可以从现有任何线程调用 `std::thread::spawn` 来创建一个新线程：

```rust
use std::thread;

fn main() {
    let handle = thread::spawn(|| {
        println!("hello concurrency world from {:?}", thread::current().id());
    });
    handle.join().unwrap();

    println!("hello concurrency world from {:?}", thread::current().id());
}
```

这段代码输出如下：

```plain
hello concurrency world from ThreadId(1)
hello concurrency world from ThreadId(0)
```

其中 `thread::spawn` 接受一个 `FnOnce` 参数开启一个新线程，
返回调用线程一个 `JoinHandle`，
我们可以利用这个 `JoinHandle` 来获取新线程对象，
或等待新线程结束。

每一个线程都有一个唯一标识 `id`，在任何地方调用 `thread::current` 来获取当前线程对象，
通过线程对象的 `id` 方法获取该线程的 `id`。


## 配置线程

我们也可以调用 `thread::Builder` 在创建线程前配置线程参数(线程名，线程栈大小)：

```rust
use std::thread;

fn main() {
    let handle = thread::Builder::new()
        .name("child".into())
        .stack_size(32 * 1024)
        .spawn(|| {
            println!("hello concurrency world from {:?}", thread::current().name());
        }).unwrap();
    handle.join().unwrap();

    println!("hello concurrency world from {:?}", thread::current().name());
}
```

这段代码输出如下：

```plain
hello concurrency world from Some("child")
hello concurrency world from Some("main")
```
