created: 2018-05-14T15:40:59+08:00
tags: [rust, concurrency]


## Barrier

`Barrier` 可以让多个线程同时都执行到在某一点后才能一起再往后执行：

```rust
use std::thread;
use std::sync::{Arc, Barrier};

fn main() {
    let mut handles = Vec::with_capacity(10);
    let barrier = Arc::new(Barrier::new(10));

    for _ in 0..10 {
        let b = barrier.clone();
        handles.push(thread::spawn(move|| {
            println!("before wait");
            b.wait();
            println!("after wait");
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
```

上面程序会在所有线程都打印出 `before wait` 后，才会在继续运行，打印 `after wait`，
这样就保证了所有线程的 `after wait` 输出在 `before wait` 输出之后。


## Condition Variables

`Condition Variables` 通常和 `Mutex` 一起使用，可以让线程挂起，直到某个条件发生后才会让线程继续执行下去：

```rust
use std::thread;
use std::sync::{Arc, Mutex, Condvar};

fn main() {
    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let pair2 = pair.clone();

    thread::spawn(move|| {
        let &(ref lock, ref cvar) = &*pair2;
        let mut started = lock.lock().unwrap();
        println!("changing started");
        *started = true;
        cvar.notify_one();
    });

    let &(ref lock, ref cvar) = &*pair;
    let mut started = lock.lock().unwrap();
    while !*started {
        started = cvar.wait(started).unwrap();
    }

    println!("started changed");
}
```

上面程序中，如果 `started` 变量是 `false`，主线程进入 `while` 循环体，
并在循环体内释放 `started Mutex` 锁后挂起线程，直到在其他线程中调用 `cvar.notify_one` 方法来唤醒主线程，
然后再次利用 `while` 循环条件判断，直到 `started` 的值为 `true` 才会继续执行循环之后的代码。


## Call Once

有时，多线程中，有些函数只允许被调用一次，通常这类函数是用来在线程间初始化一些全局变量的，
这样，无论哪个线程先调用这个函数，都会保证全局变量只会被初始化一次，
随后的其他线程的调用就忽略会这个函数体了。

```rust
use std::thread;
use std::sync::{Once, ONCE_INIT};

static mut VAL: usize = 0;
static INIT: Once = ONCE_INIT;

fn main() {
    let handle1 = thread::spawn(move || {
        INIT.call_once(|| {
            unsafe {
                VAL = 1;
            }
        });
    });

    let handle2 = thread::spawn(move || {
        INIT.call_once(|| {
            unsafe {
                VAL = 2;
            }
        });
    });

    handle1.join().unwrap();
    handle2.join().unwrap();

    println!("{}", unsafe { VAL });
}
```

上面程序运行结果取决于哪个线程第一次调用 `INIT.call_once`，
如果是 `handle1`，那么结果是 `1`，
如果是 `handle2`，那么结果是 `2`。


## 线程局部变量

rust 中用 `thread_local` 宏来初始化线程局部变量，并在线程内部用该变量的 `with` 方法来获取该变量值：

```rust
use std::cell::RefCell;
use std::thread;

thread_local! {
    static FOO: RefCell<u32> = RefCell::new(1);
}

fn main() {
    FOO.with(|foo| {
        assert_eq!(*foo.borrow(), 1);
        *foo.borrow_mut() = 2;
    });

    // each thread starts out with the initial value of 1
    thread::spawn(move|| {
        FOO.with(|foo| {
            assert_eq!(*foo.borrow(), 1);
            *foo.borrow_mut() = 3;
        });
    });

    // we retain our original value of 2 despite the child thread
    FOO.with(|foo| {
        assert_eq!(*foo.borrow(), 2);
    });
}
```
