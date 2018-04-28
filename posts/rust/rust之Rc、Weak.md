created: 2018-04-28T14:03:23+08:00
tags: [rust, 源码]

rust 语言并没有提供垃圾回收(GC, Garbage Collection )的功能，
不过它提供了最简单的引用计数包装类型 `Rc`，这种引用计数功能也是早期 GC 常用的方法，
但是引用计数不能解决循环引用，所以 rust 同时还提供了 `Weak` 类型用来避免循环引用。

## Rc、Weak 示例

首先看一下 `Rc` 的一个例子：

```rust
use std::rc::Rc;

fn main() {
    let a = Rc::new(1);
    println!("a's reference count is {}", Rc::strong_count(&a));

    let b = Rc::clone(&a);
    println!("a's reference count is {} after clone b", Rc::strong_count(&a));

    let c = Rc::clone(&a);
    println!("a's reference count is {} after clone c", Rc::strong_count(&a));
    
    println!("a = {}, b = {}, c = {}", a, b, c);
    println!("b's reference count is {}", Rc::strong_count(&b));
    println!("c's reference count is {}", Rc::strong_count(&c));
    
    drop(a);
    println!("c's reference count is {} after drop `a`", Rc::strong_count(&c));

    drop(b);
    println!("c's reference count is {}  after drop `b`", Rc::strong_count(&c));
}
```

运行结果为：

```plain
a's reference count is 1
a's reference count is 2 after clone b
a's reference count is 3 after clone c
a = 1, b = 1, c = 1
b's reference count is 3
c's reference count is 3
c's reference count is 2 after drop `a`
c's reference count is 1  after drop `b`
```

从这里可以看出，`a`、`b`、`c` 三个变量同时指向了同一个对象，
这个对象的引用计数会随着 `clone` 调用而加 1，随着 `drop` 调用而减 1，
当最后一个变量被 `drop` 时，引用计数会变为 0，进而触发释放对象被占用的堆内存。

如果 `Rc` 包装的对象是一个容器类型时，有可能会产生循环引用，比如像下面这样的类型：

```rust
struct Car {
    name: String,
    whells: RefCell<Vec<Rc<Wheel>>>,
}

struct Wheel {
    id: i32,
    car: Rc<Car>,
}
```

就有可能会出现：`Car` -> `Wheel` -> `Car` 类型的循环引用，
这个循环应用所涉及的对象的引用计数永远都不可能为0, 所占用的内存永远也不会得到释放，
直到进程结束，这样就造成了内存泄露，为了避免这种情况，rust 还提供了 `Weak` 类型，
和 `Rc` 类型来协同使用：


```rust
use std::cell::RefCell;
use std::rc::{Rc, Weak};

struct Car {
    name: String,
    whells: RefCell<Vec<Weak<Wheel>>>,
}

struct Wheel {
    id: i32,
    car: Rc<Car>,
}

fn main() {
    let car: Rc<Car> = Rc::new(
        Car {
            name: "A".to_string(),
            whells: RefCell::new(vec![]),
        }
    );
    let whell1 = Rc::new(
        Wheel {
            id: 1,
            car: Rc::clone(&car),
        }
    );
    let whell2 = Rc::new(
        Wheel {
            id: 2,
            car: Rc::clone(&car),
        }
    );

    let mut whells = car.whells.borrow_mut();
    whells.push(Rc::downgrade(&whell1));
    whells.push(Rc::downgrade(&whell2));
    drop(whells);

    for whell_weak in car.whells.borrow().iter() {
        let whell = whell_weak.upgrade().unwrap();
        println!("Whell {} owned by {}", whell.id, whell.car.name);
    }
}
```

## Rc、Weak 原理

首先我们从源码看看 `Rc` 和 `Weak` 的结构：

```rust
struct RcBox<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}

struct Rc<T: ?Sized> {
    ptr: Shared<RcBox<T>>,
}

struct Weak<T: ?Sized> {
    ptr: Shared<RcBox<T>>,
}
```

从上面可以看出：`Rc` 和 `Weak` 内存表示并没有什么不同，内部都是存放一个指向 `RcBoX` 类型的指针，
这个 `RcBoX` 类型的指针指向堆的某个地方。

`Rc` 和 `Weak` 的真正不同的地方是针对指向的 `RcBox` 内部的 `strong`、`weak` 的处理上面，
其中 `strong` 属性用来表示 `value` 对象的强引用次数(strong reference count)，
`weak` 属性用来表示 `value` 对象的弱引用次数(weak reference count，其实是弱引用次数加 1)，
对它们的操作有如下规则：

* 当初始化一个 `Rc` 时，`RcBox.strong` 和 `RcBox.weak` 都被初始化为 1
* 当执行 `Rc::clone` 时，`RcBox.strong` 加 1，
  当 `drop` 一个 `Rc` 时，`RcBox.strong` 减 1，
  如果此时 `RcBox.strong` 变为 0，就再将 `RcBox.weak` 减 1，
  如果此时 `RcBox.weak` 等于 0，就释放 `RcBox` 类型对象所占用的内存。
* 当执行 `Rc.downgrade` 时，返回一个 `Weak` 引用，并将 `RcBox.weak` 加 1,
  当 `drop` 一个 `Weak` 时，`RcBox.weak` 减 1，当 `RcBox.weak` 等于 0 时，就释放 `RcBox` 类型对象所占用的内存。
* 当执行 `Weak.upgrade` 时，如果此时 `RcBox.strong` 为 0 返回 `None`，
  否则 `RcBox.strong` 加 1，返回 `Some(Rc)`。

这样就可以通过 `Rc` 和 `Weak` 像上面协同使用就避免了循环引用了。


## Rc、Weak 限制

由于 `Rc` 和 `Weak` 并没有实现 `Send` 和 `Sync` trait，
所以这两种包装类型只能用于单线程中，不能跨线程操作，
如果需要跨线程操作，就需要用到 `std::sync::Arc` 和 `std::sync::Weak` 了。


## std::sync::Arc 和 std::sync::Weak 原理

先看一下它们的源码：

```rust
struct ArcInner<T: ?Sized> {
    strong: atomic::AtomicUsize,
    weak: atomic::AtomicUsize,
    data: T,
}

struct Arc<T: ?Sized> {
    ptr: Shared<ArcInner<T>>,
}

struct Weak<T: ?Sized> {
    ptr: Shared<ArcInner<T>>,
}
```

它们与 `std::rc::Rc` 和 `std::rc::Weak` 类同，
只是 `strong` 和 `weak` 的类型使用了线程安全的 atomic 类型，
当然这也带来了一部分性能损失。
