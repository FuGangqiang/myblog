created: 2018-04-27T22:24:49+08:00
tags: [rust, 源码]


以前一直困惑于 rust 的 `Cell` 和 `RefCell` 类型，
今天终于理解了，
主要参考了它们的源码实现和一些网上的资料：

* [reddit](https://www.reddit.com/r/rust/comments/4cvc3o/what_are_cell_and_refcell_used_for/)
* [stackoverflow](https://stackoverflow.com/questions/30275982/when-i-can-use-either-cell-or-refcell-which-should-i-choose?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa)
* [Interior mutability in Rust: what, why, how?](https://ricardomartins.cc/2016/06/08/interior-mutability)

## & 和 &mut

在讲解 `Cell` 和 `RefCell` 之前，不得不说一下 `&` 和 `&mut` 类型。

rust 提供了两种引用类型：

* `&`: shared reference，共享引用
* `&mut`: mutable reference，可变引用

rust 编译器对这两种引用类型有特别严格的限制：
同一个作用域下，对于资源对象 `A` 的引用只允许两种情况，
要么是同时存在 n 个共享引用 `&`，要么只有一个可变引用 `&mut`，
共享引用不允许对其作任何修改，可变引用可以修改。

rust 的 borrow checker 会根据这两条规则对代码做编译期检测，
进而检测出代码中可能出现的错误，但是这样的检测又过于严格，
导致有些类型的代码不能编译通过，比如：

```rust
fn main() {
    let x = 1;
    let y = &x;
    let z = &x;
    x = 2;
    y = 3;
    z = 4;
    println!("{}", x);
}
```

还有，对于 `struct` 对象，引用类型的可变性是针对整个值的，并不针对其中的某一个字段，
也就是说一个 `struct` 类型的共享引用 `&` 是不能修改这个对象的任何一个字段的，
而一个 `struct` 类型的可变引用 `&mut` 可以修改这个对象的任何一个字段，
这样 rust 的 borrow checker 在编译以下代码时也会报错：

```rust
#[derive(Debug)]
struct Point {
    x: i32,
    y: i32
}

fn main() {
    let p = Point{x: 1, y: 2};
    let p1 = &p;
    let p2 = &p;
    p1.x = 3;
    p2.x = 4;
    println!("{:?}", p);
}
```

以上两种类型的代码在 c 语言中经常使用，但是在 rust 语言中就不允许通过了，
为此，rust 语言就引入了 `Cell` 和 `RefCell` 两种 wrapper 类型来实现类似的操作。

下面，我们看一下如何用 `Cell`、`RefCell` 类型来实现上面两个例子。

## Cell 样例

例一：

```rust
use std::cell::Cell;

fn main() {
    let x = Cell::new(1);
    let y = &x;
    let z = &x;
    x.set(2);
    y.set(3);
    z.set(4);
    println!("{:?}", x);
}
```

例二：

```rust
use std::cell::Cell;

#[derive(Debug)]
struct Point {
    x: Cell<i32>,
    y: i32
}

fn main() {
    let p = Point{x: Cell::new(1), y: 2};
    let p1 = &p;
    let p2 = &p;
    p1.x.set(3);
    p2.x.set(4);
   
    println!("{:?}", p);
}
```


## RefCell 样例

例一：

```rust
use std::cell::RefCell;

fn main() {
    let x = RefCell::new(1);
    let y = &x;
    let z = &x;
    *x.borrow_mut() = 2;
    *y.borrow_mut() = 3;
    *z.borrow_mut() = 4;
    println!("{:?}", x);
}
```

例二：

```rust
use std::cell::RefCell;

#[derive(Debug)]
struct Point {
    x: RefCell<i32>,
    y: i32
}

fn main() {
    let p = Point{x: RefCell::new(1), y: 2};
    let p1 = &p;
    let p2 = &p;
    *p1.x.borrow_mut() = 3;
    *p2.x.borrow_mut() = 4;
   
    println!("{:?}", p);
}
```

## Cell 和 RefCell 有什么不同？

从上面看，`Cell` 和 `RefCell` 似乎只是表达用法上面有些不同，程序的逻辑方面似乎并没有什么不同，
那为什么 rust 同时提供这两种类型呢？

其实看了源码之后你就可以知道：`Cell` 比 `RefCell` 更轻，性能更好，用法更方便，
但是 `Cell` 只能包装 `Copy` 类型，而 `RefCell` 可以包装任何类型，
并且 `RefCell` 可以获取其内部包装对象的引用，并在运行时检测可变引用的唯一性。

它们的内部结构如下：

```rust
struct Cell<T> {
    value: UnsafeCell<T>,     // 内部对象
}

struct RefCell<T: ?Sized> {
    borrow: Cell<usize>,      // 对象引用类别和计数
    value: UnsafeCell<T>,     // 内部对象
}
```

`RefCell` 内部维护了一个包装对象的引用计数，
当 `RefCell.borrow` 获取一个共享引用时，内部引用计数加一，当获取的引用离开作用域时，内部引用计数减一，
当 `RefCell.borrow_mut` 获取一个可变引用时，首先检测引用技数是否为 0，如果为 0，正常返回，
如果不为 0，直接 panic，其实 `RefCell.borrow` 时也会做类似的检测，当已经获取了可变引用也是直接 panic，
当然为了避免 panic，我们可以用 `RefCell.try_borrow` 和 `RefCell.try_borrow_mut` 来获取一个 `Result` 类型。

## Cell 和 RefCell 的限制

因为 `Cell` 和 `RefCell` 两种类型都未实现 `Sync` trait，
所以这两种包装类型只能用于单线程中，不能跨线程操作，
如果需要跨线程操作，就需要用到 `Mutex` 和 `RwLock` 了。
