created: 2010-12-08T22:21:00+08:00
tags: [scheme]

# 第一贴: Hello World

进入 scheme 之前, 先写一个经典的 "Hello World" 程序:

```
;The first program "hello.scm"
(begin
  (display "Hello, World!")
  (newline))
```

其中:
第一行为注释, 当 scheme 遇到分号`;`时, 就会把分号到行尾的内容当作注释而不去处理.
begin-form 在 scheme 中会对其后的子表达式顺序求值, 并返回最后一个子表达式的结果.
在本例中, begin-form 有两个子表达式, 第一个是 `display` 函数, 该函数打印出其参数`<string>`; 第二个是 `newline` 函数,该函数打印一个换行符.

为了运行这个程序, 首先要运行你的 scheme.
在此, 我用的是 guile.<当然你也可以用 scheme 其他的实现>
在终端键入 "guile", 随后出现一个提示符 "guile>":

```
$ guile
guile>
```

此时, 你进入了 REPL(read-eval-print-loop), 顾名思义, read your input, evaluate it, print the result(if any).

键入:

```
guile> (load "hello.scm")
Hello, World!
```

退出你的 scheme, 键入:

```
guile> (exit)
$
```
