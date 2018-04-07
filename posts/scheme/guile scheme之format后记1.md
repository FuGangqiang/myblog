created: 2011-01-17T18:20:00+08:00
tags: [scheme]

以前曾写过一个关于 guile scheme 中 format 过程的介绍<Guile Scheme 之 format>，
但在那里只是向大家展示了使用 `format` 过程来格式化字符串的一个简单示例，
并没有详细介绍 `format` 的具体用法，本文及后续各篇再次通过一些小示例来具体介绍如何使用 `format` 过程。

## format 过程

在 guile 中使用 `format` 过程需要加载 `(ice-9 format)` 模块：

```
scheme@(guile-user)> (use-modules (ice-9 format))
```

使用 `format` 过程来产生格式化字符串比利用 `display`、`write` 和 `newline` 等过程更快捷，更方便。
调用形式：

```
(format dest fmt . arg)
```

其中：
1. `dest` 控制 `format` 过程输出字符串的方式：
    * 当 `dest` 为 `#t` 时，`format` 过程会把格式化字符串输出到 `current-output-port`；
    * 当 `dest` 为 `#f` 时，`format` 过程会把格式化字符串作为返回值返回；
    * 当 `dest` 为一个 `port` 时，format 过程会把格式化字符串输出到相应的 `port`。
2. `fmt` 为 `format` 过程控制字符串，学习它是学习 format 过程的核心，它也被称为 format 语言，format 语言一点也不 Lispy ——它的语法是基于字符，而不是符号表达式，它特别注重紧凑以至于不易于理解，这就是为什么一个复杂的 format 控制字符串被认为是程序一行中的异类。
3. `args` 会被 format 过程根据 format 控制字符串中的指令分别插入到格式化字符串的相应位置或作为其他用处。

## format 语言

format 语言是由文本和指令组成：文本将会作为格式化字符串的一部分而输出，指令则会控制 format 相应的参数如何被插入到格式化字符串中。
`fmt` 中指令均以 `~` 开头，指令格式为：

```
~ [param [, param...]] [:] [@] code
```

其中：
`code` 为一个字符，决定 `fmt` 指令的功能；
`:` 和 `@` 都是可选的指令选项，其中之一或两者同时出现会改变指令的执行方式；

可选的 `param` 被一些 `fmt` `指令应用，来增强指令的执行方式，param` 有以下形式：

* `[+/-]number`： 一个数字和其可选前缀 + 或 -；
* `'c`：一个引号和其后的一个字符，代表本字符；
* `v`：下一个对应的过程参数作为这个选项，大写 V 同样适用；
* `#`：format 过程余下的参数数量；

注：一个 `fmt` 指令可以有数个 `param`，而 `param` 间有逗号 `,` 分割，若只是指定第三个 `param` 选项，可用 `,,param` ，只需在其前加两个逗号来表示其为第三个选项，前两个省略不写为默认值。
 
看完上面的介绍，你可能会是一头雾水，那让我们通过后续各篇中的例子来学习各个 `fmt` 指令。
