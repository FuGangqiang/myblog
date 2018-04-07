created: 2010-12-04T14:33:00+08:00
tags: [scheme]

guile 中的 format 函数与 c 语言中的 printf 函数作用相同, 但是远比它强大, 其类似于 common lisp 中的 format 函数, 但是他们并不相同, guile 中的 format 函数并没有 common lisp 中的全部特性.

guile 默认的 format 函数为 simple-format, 它只支持 ~A 和 ~S 格式化字符, ~A 使用 display 函数, 而 ~S 使用 write 函数. 若想用 format 的全部特性, 需加载 (ice-9 format) 模块:

```
guile> (use-modules (ice-9 format))
```

这样我们就可以用 format 函数来进行格式化字符串了.

我们先看一个例子:
在此, 我们有一个列表, 我们想用逗号 `,` 分割打印列表中的各个元素,比如:
有一列表 `(1 2 3 4)`，
要打印出 `1, 2, 3, 4`，
下面是 format 实现:

```
guile> (define xs '(1 2 3 4))
guile> (format #f "~{~a~^, ~}" xs)
"1, 2, 3, 4"
```

下面是用 let 语句来实现同样的功能:

```
guile> (let loop ((ls  xs))
...        (if (null? (cdr ls))
...            (format #f "~a" (car ls))
...            (string-append (format #f "~a, " (car ls))
...                           (loop (cdr ls)))))
"1, 2, 3, 4"
```

从这两个例子我们可以看出 format 函数为我们提供了一种便捷的方式来格式化字符串.
