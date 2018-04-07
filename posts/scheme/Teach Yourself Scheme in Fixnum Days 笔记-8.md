created: 2010-12-24T22:12:00+08:00
tags: [scheme]

# 第七贴：宏

本文中的 guile 提示符前半部分为 `scheme@(guile-user)>` ，后半部分为 `guile>`

这是因为在写本文过程中，我改变了guile 中的提示符

方法如下：

在 `~/.guile` 文件内添加如下：

```
(use-modules (system repl common))
(define (repl-default-prompt-set! prompt)
  (repl-default-option-set! 'prompt prompt))
(repl-default-prompt-set! "guile>")
```

闲话少序，直奔主题：
 
用户可以自定义宏来生成属于他们自己的特殊语句。一个宏就是一个与宏转换器相关联的名字<symbol>。当 scheme 碰到一个宏表达式时<语句头元素是一个宏>，首先利用宏后的子语句作为参数来调用宏转换器，然后计算转换后的结果。
 
宏实际上就是代码间的一个纯文本转换机制，而这种转换机制对于简化经常使用的文本模式特别有用。
 
`define-macro` 是用来定义宏的一个特殊语句<这是一种scheme传统宏定义方式，还有卫生宏定义，这里并不介绍卫生宏>。
比如，如果你的scheme版本没有条件语句`when`，我们就可以通过以下代码定义它：

```
scheme@(guile-user)> (define-macro (when test . body)
...                     (list 'if test
...                        (cons 'begin body)))
```

这样，我们就得到我们想要的宏 `when`：

```
scheme@(guile-user)> (when (< 1 2)
...                     (display "Fisrt line") (newline)
...                     (display "Second line") (newline))
Fisrt line
Second line
```

定义宏 `when` 时定义了一个 `when` 转换器来把 `when` 语句转换为等价的 `if` 语句，上面的代码会被转换为：

```
scheme@(guile-user)> (if (< 1 2)
...                     (begin (display "First line") (newline)
...                            (display "Second line") (newline)))
First line
Second line
```

我们可以用宏 `when` 来定义一个 `unless` 宏：

```
scheme@(guile-user)> (define-macro (unless test . body)
...                     (cons 'when
...                           (cons (list 'not test) body)))
```

同样，使用宏 `unless`:

```
scheme@(guile-user)> (unless (not (< 1 2))
...                     (display "First line") (newline)
...                     (display "Second line") (newline))
First line
Second line
```

## Specifying the expansion as a template

一个宏转换器以一些符号表达式为参数来生成需要使用的符号表达式语句，通常结果是一个列表。
在 `when` 宏的例子中，其产生结果列表是由下面产生的：

```
(list 'if test
   (cons 'begin body))
```

在宏 `when` 中，`test` 被绑定为宏的第一个语句 `(< 1 2)`，
`body` 被绑定为 `((display "First line") (newline) (display "Second line") (newline))`.

宏输出语句有可能相当复杂，为了使宏定义更具有可读性，宏定义引进了 `backquote` 来设定一个模板。

例如，下面的表达式

```
(list 'if test
   (cons 'begin body))
```

可被下面的

```
`(if ,test
   (begin ,@body))
```

替换。
因此，我们可以有一个更具有可读性的宏 `when` 定义：

```
(define-macro (when test . body)
   `(if ,test
      (begin ,@body)))
```

注意：当我们一看到这个模板格式时，就可以看出宏 `when` 输出的语句具体的样子。

`backquote`<`\``>为一个列表引进一个模板，在这个列表模板中，紧挨 `,` 和 `,@` 后的语句将会被特殊对待，
紧挨着 `,` 后语句将会被替换为该语句的执行计算结果，而紧挨着 `,@` 后的语句将会被替换为该语句执行计算结果的各元素的排列。

例如：

```
scheme@(guile-user)> (define a '(1 2 3 4))
scheme@(guile-user)> (define b '(1 2 3 4))
scheme@(guile-user)> `(10 20)
(10 20)
scheme@(guile-user)> `(10 20 ,a)
(10 20 (1 2 3 4))
scheme@(guile-user)> `(10 20 ,(+ 1 2))
(10 20 3)
scheme@(guile-user)> `(10 20 ,@a)
(10 20 1 2 3 4)
scheme@(guile-user)> `(10 20 ,@(map + a b))
(10 20 2 4 6 8)
scheme@(guile-user)> `(10 20 ,@(map * a b))
(10 20 1 4 9 16)
```

在我们上面的简易宏定义后，只要我们知道 `test` 和 `body` 被绑定的表达式，我们就不难推出宏 `when` 转换器转换的结果。

## Avoiding variable capture inside macros
 
我们可以像下面定义一个两个参数的 `or` 宏：

```
guile>(define-macro (my-or x y)
...     `(if ,x ,x ,y))
guile>(my-or 1 2)
1
guile>(my-or #f 2)
2
```

上面的宏 `my-or` 有一个错误：如果第一个参数值为真，那么这个参数会被计算两次，一次是在 `if-test` 中，一次是在 `then-branch` 中。这个错误会产生令人意想不到的副作用，比如：

```
guile>(my-or (begin (display "doing first argument")
...                 (newline)
...                 #t)
...     2)
doing first argument
doing first argument
#t
```

其中：`begin` 语句被计算了两次。

我们可以重写宏 `my-or` 来避免这个问题：

```
guile>(define-macro (my-or x y)
...     `(let ((temp ,x))
...        (if temp temp ,y)))
guile>(my-or (begin (display "doing first argument")
...                 (newline)
...                 #t)
...     2)
doing first argument
#t
```

但是，这个定义并没有解决所有问题，还有一个相当隐蔽的 bug，也是经常容易出错的，就是用上面定义的宏 `my-or` 时，第二个参数也包含 `temp` 标识符，从而与宏 `my-or` 定义中的局部变量 `temp` 标识符名字重名：

```
guile>(define temp 3)
guile>(my-or #f temp)
#f
```

很明显，在上例中，`(my-or #f temp)` 的值应该为 `3`，而结果却为 `#f`，也即是 `my-or` 第一个参数的值。这里主要是因为 `(my-or #f temp)` 第二个参数 `temp` 与 `my-or` 宏定义中的局部变量 `temp` 重名，而当宏 `my-or` 展开时，全局 `temp` 标识符被解释为宏 `my-or` 体内的局部变量标识符，进而被绑定为 `#f`。
 
为了避免这个 bug，我们应该小心选用宏定义内的局部变量标识符，我们可以使用一些偏僻的变量命名，例如，可以用 `+temp` 来代替上例中的 `temp` 局部变量标识符：

```
guile>(define-macro (my-or x y)
...     `(let ((+temp ,x))
...        (if +temp +temp ,y)))
guile>(or #f temp)
3
```

但是这只是换汤不换药，只是作者自认为使用宏 `my-or` 的用户不会在其第二个参数中用 `+temp` 标识符，但是这并没有避免相同的问题。
还有一种根治这个 bug 的方法，就是用 `gensym` 过程来生成独一无二的标识符：

```
(define-macro (my-or x y)
  (let ((temp (gensym)))
     `(let ((,temp ,x))
         (if ,temp ,temp ,y))))
```

每一次调用 `gensym` 过程时都会返回一个与所有环境中的所有标识符都不相同的标识符，进而不会产生标识符名字重名的现象。

## fluid-let

下面是一个更复杂的宏，`fluid-let`，这个宏可以临时改变一部分变量的值来产生动态作用域的效果，例如，给定一个 `fluid-let` 语句如下：

```
(fluid-let ((x 9) (y (+ y1)))
   (+ x y))
```

我们想要这个宏最终转换为：

```
(let ((OLD-X x) (OLD-Y y))
   (set! x 9)
   (set! y (+ y 1))
   (let (((RESULT (begin (+ x y)))))
      (set! x OLD-X)
      (set! y OLD-Y)
      RESULT))
```

我们将要使用 `gensym` 过程来避免 `OLD-X`、`OLD-Y`和`RESULT`标识符重名现象，请看如下定义：

```
(define-macro (fluid-let xexe . body)
  (let ((xx (map car xexe))
        (ee (map cadr xexe))
        (old-xx (map (lambda (ig) (gensym)) xexe))
        (result (gensym)))
     `(let ,(map (lambda (old-x x) `(,old-x ,x))
                 old-xx xx)
        ,@(map (lambda (x e)
                  `(set! ,x ,e))
               xx ee)
        (let ((,result (begin ,@body)))
          ,@(map (lambda (x old-x)
                   `(set! ,x ,old-x))
                 xx old-xx)
          ,result))))
```
