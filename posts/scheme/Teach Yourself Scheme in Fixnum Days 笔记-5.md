created: 2010-12-15T23:11:00+08:00
tags: [scheme]

# 第五贴：词法变量

今天开始试用 `guile 1.9.13` (guile2.0 的不稳定版本)，以后的笔记都会使用这个版本，直到 `guile2.0` 发行，虽然不再是以前的 `guile1.8.7` 了，不过没关系，对于这个笔记上的例子是都可以运行的，而用户界面唯一变得就是 guile repl 中提示符变为:`scheme@(guile user)>` 了。


scheme 变量的作用域是属于词法作用域`lexical scope`，即它们在程序代码中只对特定范围的代码结构可见。
前面我们学习到的变量有些是全局变量，对此也不例外，全局变量的作用域是整个程序，也是一种特定的作用范围。
其实我们已经看到过一些词法变量，只是当时没指明，它们就是 lambda 过程参数，
每当过程被调用时，这些变量就会被绑定为给定的值，而它们的作用域就是这个过程体。

例如：

```
scheme@(guile-user)> (define x 9)
scheme@(guile-user)> (define add2 (lambda (x) (+ x 2)))
scheme@(guile-user)> x
9
scheme@(guile-user)> (add2 3)
5
scheme@(guile-user)> (add2 x)
11
scheme@(guile-user)> x
9
```

这里有一个全局变量 `x`，还有一个局部变量 `x`，就是过程 `add2` 中看到的那个。
全局变量 `x` 的值一直是 `9`。第一次调用 `add2` 过程时，局部变量 `x` 会被绑定为 `3`，
而第二次调用 `add2` 时，局部变量 `x` 被绑定为全局变量 `x` 的值，即 `9`。
当 `add2` 过程调用结束时，全部变量 `x` 仍然是 `9`。

而 `set!` 语句可修改这种词法变量值：

```
scheme@(guile-user)> (set! x 20)
```

上面将全局变量 `x` 的值 `9` 修改为 `20`，因为对于 `set!` 来说，全局变量 `x` 是可见的。
如果 `set!` 语句出现在 `add2` 过程体内，那局部变量 `x` 就会被修改：

```
scheme@(guile-user)> (define add2
...                     (lambda (x)
...                        (set! x (+ x 2))
...                        x))
scheme@(guile-user)> (add2 x)
22
```

这里 `set!` 语句将局部变量 `x` 加上 `2`，并且将返回的值再绑定到局部变量 `x`。
(从结果来看，我们无法区分这个过程和前面定义的 `add2` 过程)。
我们可以像前面那样使用全局的 `x` 作为参数来调用 `add2`：

```
scheme@(guile-user)> (add2 x)
22
```

注意：这里的全局变量 `x` 的值为 `20`，而不是 `9` 了!

`add2` 过程内的 `set!` 语句只能改变局部变量 `x` 的值。
尽管局部变量 `x` 被绑定为全局变量 `x` 的值，但后者不会因为 `set!` 改变局部变量 `x` 的值而受影响。

```
scheme@(guile-user)> x
20
```

注意：前面讨论的局部变量和全局变量使用了同样的标识符 `x`。
在代码中，这个 `x` 标识符指的就是词法闭包中的局部 `x` 变量，它会使闭包外的全局变量 `x` 的值不可见。
比如，在 `add2` 过程中，参数 `x` 的出现，就会使全局变量 `x` 的值不可见，在 `add2` 过程体中，
所有对 `x` 的引用，都是指 `add2` 过程参数变量 `x`，而不是全局变量 `x`。

```
scheme@(guile-user)> (define counter 0)
scheme@(guile-user)> (define next-counter
...                     (lambda ()
...                        (set! counter (+ counter 1))
...                        counter))
```

上面 `next-counter` 过程没有参数，它没有引入局部变量，因此就不会使一些全局变量不可见。
在每次调用 `next-counter` 时，它会修改全局变量 `counter` 的值，对其加 `1`，然后返回修改后的值。
下面就是对 `next-counter` 调用时的结果：

```
scheme@(guile-user)> (next-counter)
1
scheme@(guile-user)> (next-counter)
2
scheme@(guile-user)> (next-counter)
3
scheme@(guile-user)> (next-counter)
4
```

## let 和 let*

局部变量也可以不通过显式的创建过程产生。特殊语句 `let` 可以创建一个局部变量列表，以便在 `let` 体中使用:

```
scheme@(guile-user)> (let ((x 1)
...                        (y 2)
...                        (z 3))
...                     (list x y z))
(1 2 3)
```

和 `lambda` 语句一样，这里局部变量 `x` 会使全局变量 `x`<值为 `20`> 不可见。

局部变量 `x`、`y`、`z` 被赋值时<`x=1,y=2,z=3`>，不被作为 `let` 体的一部分。
因此，在局部变量被赋值时，所有对 `x` 的引用都为全局变量 `x`，而不是局部变量 `x`。

```
scheme@(guile-user)> (let ((x 1)
...                        (y x))
...                     (+ x y))
21
```

上面结果是因为局部变量 `x` 被赋值为 `1`，而 `y` 被赋值为全局变量 `x` 的值 `20`。

上面的 `x`,`y` 赋值没有先后顺序，有时为了方便，我们想要先对局部变量 `x` 赋值为 `1`，
然后再引用这个 `x` 变量对局部变量 `y` 赋值，scheme 中的 `let*` 语句就是这种作用：

```
scheme@(guile-user)> (let* ((x 1)
...                         (y x))
...                     (+ x y))
2
```

这里对局部变量 `y` 赋值时使用变量 `x` 就是前面刚创建好的局部变量 `x`。
这个语句完全等价于下面的 `let` 语句嵌套，实际上，`let*` 语句就是 `let` 语句的嵌套缩写。

```
scheme@(guile-user)> (let ((x 1))
...                     (let ((y x))
...                        (+ x y)))
2
```

通过 `let` 语句，我们也可以把一个过程作为值赋给一个局部变量，因为过程在 scheme 中，过程是第一等公民：

```
scheme@(guile-user)> (let ((cons (lambda (x y)
...                                 (+ x y))))
...                     (cons 1 2))
3
```

上面这个 `let` 体中，局部变量 `cons` 被赋值为一个过程，使其两个参数进行相加，在这个 `let` 体中，对 `cons` 的引用就不会是全局 `cons` 过程那样来创建点对，而是对其两个参数相加，返回结果。
而在 `let` `体的外面，cons` 仍还是用来创建点对：

```
scheme@(guile-user)> (cons 1 2)
(1 . 2)
```
