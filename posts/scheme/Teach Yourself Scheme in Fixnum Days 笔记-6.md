created: 2010-12-16T22:22:00+08:00
tags: [scheme]

# 第六贴：递归

在一个过程体内不仅可以调用其他过程，也可以调用自身（包括间接调用），而后者被称为递归过程：

```
scheme@(guile-user)> (define factorial
...                     (lambda (n)
...                        (if (= n 0)
...                             1
...                             (* n (factorial (- n 1))))))
```

上面这个递归过程是用来计算一个数的阶乘。
如果这个数为 `0`，过程返回结果为 `1`；
如果这个数为其他的自然数 `n`，这个过程就会先调用自身求出 `n-1` 的阶乘，然后把这个结果乘以 `n`，最后返回计算的乘积。
 
递归过程可以相互的调用。
下面是一个判断奇偶性的两个过程，它们就是相互调用来定义的：

```
scheme@(guile-user)> (define is-even?
...                     (lambda (n)
...                        (if (= n 0)
...                            #t
...                            (is-odd? (- n 1)))))
scheme@(guile-user)> (define is-odd?
...                     (lambda (n)
...                        (if (= n 0)
...                            #f
...                            (is-even? (- n 1)))))
scheme@(guile-user)> (is-even? 4)
#t
scheme@(guile-user)> (is-odd? 5)
#t
```

## letrec

如果要把上面的过程绑定到局部变量，我们可以用 `letrec` 语句：

```
scheme@(guile-user)> (letrec ((local-even? (lambda (n)
...                                             (if (= n 0)
...                                                 #t
...                                                 (local-odd? (- n 1)))))
...                            (local-odd? (lambda (n)
...                                             (if (= n 0)
...                                                 #f
...                                                 (local-even? (- n 1))))))
...                       (list (local-even? 4) (local-odd? 5)))
(#t #t)
```

`可以看出，letrec` 是专为相互递归定义而设立的语句，从名字`let recursion`就可以看到其作用。

注意：
上面定义的 `local-odd?` 和 `local-even?` 函数只能在 `letrec` 体内可见，因为它们为局部变量；
不能用 `let` 或者 `let*` 来替换 `letrec` 语句，因为如果用 `let` `语句的话，let` 语句就会认为：在定义 `local-even?` 的时候，把所调用 `local-odd?` 函数理解为全局变量，而在全局变量中，并没有定义 `local-odd?` 函数，因此会报错，定义 `local-odd?` 函数是同样如此；如果用 `let*` 语句的话，`let*` 会顺序执行其参数定义，在定义第一个参数 `local-even?` 时，因为并没有 `local-odd?` 的定义，所以也会报错。

## named let

使用 `letrec` 来定义的递归过程可以来实现循环，让我们看看下面用 `letrec` 定义的一个连续循环打印 `10` 个数的函数定义：

```
scheme@(guile-user)> (letrec ((countdown (lambda (i)
...                                             (if (= i 0)
...                                                 'end
...                                                 (begin (display i)
...                                                        (newline)
...                                                        (countdown (- i 1)))))))
...                       (countdown 10))
10
9
8
7
6
5
4
3
2
1
end
```

在 scheme 中，有一个更简单的定义上述功能的语句： `named let` 语句：

```
scheme@(guile-user)> (let countdown ((i 10))
...                     (if (= i 0)
...                        'end
...                        (begin (display i)
...                               (newline)
...                               (countdown (- i 1)))))
10
9
8
7
6
5
4
3
2
1
end
```

上面的这个定义与前面用 `letrec` 语句定义的 `countdown` 局部过程是完全等价的。

## iteration

上面定义的 `countdown` 是递归过程，scheme 没有 C 语言中的 `for`、`while` 循环、迭代语句，而是用递归过程来实现循环、迭代的。
 
在其他语言中，用递归来实现循环，从而消耗很大的内存，scheme 则对上面类似的递归调用特殊处理，使其内存消耗为一个常量，这个主要是因为 scheme 支持尾调用。如果你细心观察，在 `countdown` 过程体中，每次调用自身都是尾调用或者是在过程体最后的一个表达式，这就是尾调用，而 scheme 中的尾调用不会消耗额外的内存空间，所以，在 scheme 中，请放心用递归来实现循环。
 
下面是一个尾递归<也是尾调用>的例子：
 
```
scheme@(guile-user)> (define list-position
...                     (lambda (o l)
...                        (let loop ((i 0) (l l))
...                           (if (null? l)
...                               #f
...                               (if (eqv? (car l) o)
...                                   i
...                                   (loop (+ i 1) (cdr l)))))))
scheme@(guile-user)> (list-position 'c '(a b c d))
2
```

这个函数是寻找一个对象 `o` 在给定列表 `l` 中的索引位置<索引从 `0` 开始>，如果 `o` 不在 `l` 列表中，就返回` #f`。
 
下面也是一个尾递归过程：

```
scheme@(guile-user)> (define list-reverse!
...                     (lambda (s)
...                        (let loop ((s s) (r '()))
...                           (if (null? s)
...                               r
...                               (let ((d (cdr s)))
...                                   (set-cdr! s r)
...                                   (loop d s))))))
scheme@(guile-user)> (define ls '(1 2 3 4))
scheme@(guile-user)> (list-reverse! ls)
(4 3 2 1)
scheme@(guile-user)> ls
(1)
```

它的作用是返回给定列表的倒序列表，但是所给定列表会发生改变，若不想改变原列表，请看下面的定义：

```
scheme@(guile-user)> (define list-reverse
...                     (lambda (ls)
...                        (let loop ((ls ls) (xs '()))
...                           (if (null? ls)
...                              xs
...                              (loop (cdr ls)
...                                    (cons (car ls) xs))))))
scheme@(guile-user)> (define ls '(1 2 3 4))
scheme@(guile-user)> (list-reverse ls)
(4 3 2 1)
scheme@(guile-user)> ls
(1 2 3 4)
```

## map and for-each

为了对列表各个元素重复执行相同的动作<过程>，scheme 提供了一种特殊形式就是 `map` 和 `for-each`。
 
`map` 过程第一个参数为一个函数，该函数以其后列表元素为参数，所得计算结果组成的列表为 `map` 的返回值，比如：

```
scheme@(guile-user)> (define add2
...                     (lambda (x)
...                        (+ x 2)))
scheme@(guile-user)> (map add2 '(1 2 3 4))
(3 4 5 6)
```

上面 map 过程：
第一个参数为函数 `add2`， `add2` 的作用为对其参数加`2`，返回其结果；
第二个参数为列表 `'(1 2 3 4)`；
`map` 的作用就是使 `add2` 以其第二个参数列表各个元素为参数:

```
(add2 1)
(add2 2)
(add2 3)
(add2 4)
```

最后在以上面的结果所组成的列表作为 `map` 函数的返回值。
其实，`map` 就好像数学中的一一映射，对给定列表执行相同的函数，得到一个新的函数。

`for-each` 与 `map` 相似，只是 `map` 会把这个列表返回，而 `for-each` 并不返回任何值。
`for-each` 函数主要是为了得到函数的副作用：

```
scheme@(guile-user)> (map display '("abc" "123" "def"))
abc123def(#<unspecified> #<unspecified> #<unspecified>)
scheme@(guile-user)> (for-each display '("abc" "123" "def"))
abc123defs
```

`display` 的作用是打印出其参数，但不返回任何值`#<unspecified>`
所以上面
`map` 先打印出 `abc123def`，而后将其返回值`(#<unspecified> #<unspecified> #<unspecified>)`显示在后面。
`for-each` 只是将 `abc123def` 打印出来，并不返回任何值。
 
`map` 和 `for-each` 也可以多参数函数为参数，但是也要有相应的与过程参数个数相同的列表个数：

```
scheme@(guile-user)> (map cons '(1 2 3) '(10 20 30))
((1 . 10) (2 . 20) (3 . 30))
scheme@(guile-user)> (map + '(1 2 3) '(10 20 30))
(11 22 33)
```
