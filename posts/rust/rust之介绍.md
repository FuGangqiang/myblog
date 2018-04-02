created: 2015-03-28T12:49:00+08:00
tags: [rust]


## 语言特性

接触 rust 语言有一段时间了，当初看到它时，就被这货给震撼了，
原来系统级的语言也可以这样玩，语言特性都太喜欢了：

* llvm based
* minimal runtime
* zero-cost abstractions
* memery safty
* module system
* trait-based generics
* algebraic data types
* pattern match
* type inference
* expression based
* lifetime checker
* concurrency
* hygenic macro
* attributes
* error handling
* simple c ffi
* unicode char and string
* immutable by default
* implicit return / expressions
* build tool and package manager: cargo

rust 竟然可以做到不牺牲运行速度，还可以像动态语言那样灵活，好久没有被颠覆语言三观了！:)，
和当时学 python、scheme 和 haskell 这三种语言一样，开阔了视野。

rust 语言提供这么多语言特性，同时也使学习成本增加了许多，
如果你熟悉 haskell 和 scheme 就会发现它们许多好的特性都被 rust 继承下来了，
基于表达式、模式匹配、类型系统、模块系统、卫生宏、...，学习起来就容易许多。


## hello world

好了，不多说了，先上一个 `hello world` 代码：

```rust
fn main() {
    println!("Hello World!");
}
```


## expression based

if 控制结构是一个 expression（表达式），而不是一个 statement（语句）:

```rust
fn main() {
    let x = -5;
    let y = if x >= 0 { x } else { -x };
    println!("y == {}", y);
}
```


## pattern match

match 控制结构也是一个表达式，可以作为 println 宏的参数：

```rust
fn main() {
    let p: (isize, isize) = (3, 4);
    println!("{}", match p {
        (0, 0) => "原点",
        (_, 0) => "在x轴上",
        (0, _) => "在y轴上",
        _      => "在其他区域",
    });
}
```


## module system

一个 crate 可以定义数个 module，在 module 中也可以嵌套定义属于自己的 module，
下面是官方举的一个例子：

```rust
mod english {
   mod greetings {
   }

   mod farewells {
   }

mod japanese {
   mod greetings {
   }

   mod farewells {
   }
```


## trait-based generics

rust 的 trait 与 haskell 的 type class 很像，也提供了相似的范型：

```rust
use std::fmt::Debug;

fn bar<T, K>(x: T, y: K)
    where T: Clone,
          K: Clone + Debug
{

    x.clone();
    y.clone();
    println!("{:?}", y);
}
```


## documention

rust 可以通过 rustdoc 命令将源码中的注释导出来，
而源码中的注释是基于 MarkDown 格式的。

```rust
/// The `Option` type. See [the module level documentation](../) for more.
enum Option<T> {
    /// No value
    None,
    /// Some value `T`
    Some(T),
}
```


## error handling

错误处理大量使用 `Result<T, E>` 和 `Option<T>` 类型，同时使用宏减少代码量：

```rust
use std::fs::File;
use std::io;
use std::io::prelude::*;

struct Info {
    name: String,
    age: i32,
    rating: i32,
}

fn write_info(info: &Info) -> io::Result<()> {
    let mut file = try!(File::open("my_best_friends.txt"));

    try!(writeln!(&mut file, "name: {}", info.name));
    try!(writeln!(&mut file, "age: {}", info.age));
    try!(writeln!(&mut file, "rating: {}", info.rating));

    return Ok(());
}
```


## test and benchmark

```rust
pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[cfg(test)]
mod tests {
    use super::add_two;

    #[test]
    fn it_works() {
        assert_eq!(4, add_two(2));
    }
}
```


## memery safty

rust 为了内存安全，引入了 ownership 和 lifetime 机制，将 c 中单一概念的指针类型分为以下几种：

* References
    * `& T`
    * `& mut T`
* Raw Pointer
    * `* const T`
    * `* mut T`
* Box
    * `Box<T>`
* Rc
    * `Rc<T>`
    * `Weak<T>`
* Cow
* Cell
    * `Cell<T>`
    * `RefCell<T>`
	* `UnsafeCell<T>`
* Synchronous
    * `Arc<T>`
    * `Weak<T>`
    * `Mutex<T>`
    * `RwLock<T>`

如果你有 c 内存模型方面的知识，学习 rust 的这些指针类型应该可以很快入手，
如果你从高级语言像 python、ruby 之类转过来的话，学习这些会有些吃力。


## hygenic macro

比 c 中的宏更强大，与 scheme 相同，都是卫生宏:

```rust
macro_rules! log {
($msg:expr) => ({
        let state: i32 = get_log_state();
        if state > 0 {
            println!("log({}): {}", state, $msg);
        }
    });
}

fn main() {
    let state: &str = "reticulating splines";
    log!(state);
}
```

## 更多

参考官方文档 [Rust Book](http://doc.rust-lang.org/book/README.html) 和 [Rust References](http://doc.rust-lang.org/reference.html)。
