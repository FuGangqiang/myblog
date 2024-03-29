created: 2022-08-01T14:34:00+08:00
tags: [c, 源码, 编译原理]


## 什么是 `C4`

`C4` 是 `C in four functions` 的缩写，
顾名思义，它仅仅用 4 个函数就实现了一个 `C` 语言，
具体来说，是实现了一个 `C` 语言的解释器，当然只是实现了一个 `C` 语言的子集。

它的代码仓库在：[https://github.com/rswier/c4](https://github.com/rswier/c4)。

看代码，仅仅 500 多行，
更厉害之处在于它实现了自举，
也就是说，它可以用它自己来编译解释它自己的实现代码，
可以说麻雀虽小，五脏俱全，是学写编译原理的绝佳范例。


## 如何阅读理解 `C4` 代码

作者对代码进行了极致精简，
代码行数以至于仅有 500 多行，
造成了理解上的困难，
不过网上已经有了各种 `C4` 源码分析的文章，
经过一番搜索，
发现其中最佳的一系列分析是[https://github.com/lotabout/write-a-C-interpreter](https://github.com/lotabout/write-a-C-interpreter)，
它是国内的一个博主写的。


## 伪自举

虽说 C4 号称实现了自举，但感觉并不是真的自举，因为 C4 里面的打印函数是直接链接的最初编译器自带的 printf 代码，
自己并未真正实现这些。
