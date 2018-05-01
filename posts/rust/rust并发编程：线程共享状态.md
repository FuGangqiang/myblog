created: 2018-05-01T18:00:45+08:00
tags: [rust, concurrency]

线程间是经常需要共享数据的，
在 c/c++ 语言中，如果程序编写不当，很容易产生难以被发现的数据竞争 bug，
而 rust 的 `Send`/`Sync` trait 让编译器强制检测代码，
并保证只要程序编译通过，就不会有数据竞争问题，从而实现了线程的并发安全。


## Send 和 Sync

`Send` trait 表明拥有该类型的变量的所有权可以从一个线程传递给另一个线程，否则不能传递；
`Sync` trait 表明拥有该类型的变量可以被多个线程同时引用，否则不能引用。

rust 中的大多类型是 `Send` 类型的，但是有些类型不是，比如 `Rc` 类型，
该类型变量只是一个指针，如果允许 `Send`，就会导致多个线程同时指向了同一块内存，
而它的实现方式对其内部的引用计数的修改又没有做任何线程同步处理，因此会出现数据竞争问题，
所以标准库将 `Rc` 标记为非 `Send` 类型，
与它相对的 `Arc` 类型的实现方式做了线程同步处理，因此 `Arc` 被标记为 `Send` 类型。

rust 中的大多类型也都是 `Sync` 类型，但是有些类型不是，比如 `Cell` 和 `RefCell` 类型，
因为这些类型具有内部可变性，如果在多个线程共享这些类型的变量并对其进行修改，
由于它们的实现方式没有做任何线程同步处理，就会导致数据竞争问题，
所以标准库将 `Cell` 和 `RefCell` 都标记为非 `Sync` 类型，
与它们相对的 `Mutex` 和 `RwLock` 考虑了线程同步，因此 `Mutex` 和 `RwLock` 都被标记为 `Sync` 类型。


## Arc 共享数据

由于 `Arc` 是 `Sync` 类型，所以我们可以在线程间共享 `Arc` 类型，代码如下：

```rust
use std::thread;
use std::sync::Arc;

fn main() {
    let nums = Arc::new(vec![1, 2, 3]);
    let mut handles = vec![];

    for _ in 0..10 {
        let nums = Arc::clone(&nums);
        let handle = thread::spawn(move || {
                println!("the nums is {:?}", nums);
            });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("the nums is {:?}", nums);
}
```


## 修改 Arc 共享数据

与 `Rc` 类型类似，`Arc` 类型并不具有内部可变性，因此是不能对其包装对象进行修改的，
因而，rust 语言标准库也提供了对应的具有内部可变性的类型 `Mutex` 和 `RwLock`，
它们不像 `Cell` 和 `CellRef` 类型，它们的修改是考虑线程同步处理的，
因此利用它们可以在线程间对共享数据进行修改的。


### Mutex 排它锁

`Mutex` 排它锁允许共享数据在一个时刻只能被一个线程访问、修改，代码如下：

```rust
use std::thread;
use std::sync::{Mutex, Arc};

fn main() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());
}
```

### RwLock 读写锁

`RwLock` 读写锁则只允许同一时刻一个线程进行写操作或多个线程同时进行读操作，代码如下：

```rust
use std::thread;
use std::sync::Arc;
use std::sync::RwLock;

fn main() {
    let counter = Arc::new(RwLock::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.write().unwrap();
            *num += 1;
            println!("Write: {}", num);
        });
        handles.push(handle);
    }

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let num = counter.read().unwrap();
            println!("Read: {}", num);
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.read().unwrap());
}
```
