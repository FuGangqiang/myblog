created: 2018-05-02T20:08:06+08:00
tags: [rust, concurrency]

rust 不仅可以利用锁来在线程间共享状态，还可以利用消息传递的方式来在线程间共享状态。


## Multi Producer Single Consumer FIFO Queue

调用 `std::mpsc::channel` 会返回一个 tuple 类型，`(Sender, Receiver)`，
我们可以利用 `Sender` 在多个线程向通道(channel)里面里面发送消息，
然后可以在一个线程里面利用 `Receiver` 接收这些消息。

```rust
use std::sync::mpsc;
use std::thread;

fn main() {
    let (tx, rx) = mpsc::channel();

    let tx1 = mpsc::Sender::clone(&tx);
    thread::spawn(move || {
        let vals = vec![
            String::from("hi"),
            String::from("from"),
            String::from("the"),
            String::from("thread"),
        ];

        for val in vals {
            tx1.send(val).unwrap();
        }
    });

    thread::spawn(move || {
        let vals = vec![
            String::from("more"),
            String::from("messages"),
            String::from("for"),
            String::from("you"),
        ];

        for val in vals {
            tx.send(val).unwrap();
        }
    });

    for received in rx {
        println!("Got: {}", received);
    }
}
```
