created: 2010-12-09T22:43:00+08:00
tags: [scheme]

# 第二贴:数据类型

scheme 有丰富的数据类型:一些属于简单类型, 一些属于复合类型.

## 简单类型

### Booleans(布尔型/是非型)

scheme 中的布尔型有 `#t`(真/是) 和 `#f`(假/非), scheme 有一个判断一个值是否为布尔类型的函数 `boolean?`:

```
guile> (boolean? #t)
#t
guile> (boolean? "Hello, World!")
#f
```

`not` 函数对其参数(被对待为布尔类型)取否:

```
guile> (not #f)
#t
guile> (not #t)
#f
guile> (not "Hello, World!")
#f
```

scheme 规定任何非 `#f` 值为真.

### numbers(数值)

scheme 数值类型有整数, 有理数, 实数和复数. 一个整数为有理数, 一个有理数为实数, 一个实数为复数, 总之, 这几个类型全部归为数值类型.

```
guile> (number? 42)
#t
guile> (number? #t)
#f
guile> (complex? 2+3i)
#t
guile> (real? 2+3i)
#f
guile> (real? 3.1416)
#t
guile> (real? 22/7)
#t
guile> (rational? 2+3i)
#f
guile> (rational? 22/7)
#t
guile> (rational? 3.1416)
#t
guile> (integer? 22/7)
#f
guile> (integer? 42)
#t
```

scheme 数值默认书写形式为 10 进制形式(`#d`可不写), 但也可写为二进制(开头为`#b`)、八进制(开头为`#o`)和十六进制(开头为`#`x)形式．

`eqv?` 可以用来判断两个数值是否相等:

```
guile> (eqv? 42 42)
#t
guile> (eqv? 42 #f)
#f
guile> (eqv? 42 42.0)
#f
```

但是我们通常用 `=` 函数来判断两个已知类型为数值的值是否相等:

```
guile> (= 42 42)
#t
guile> (= 42 #f)
ABORT: (wrong-type-arg)
guile> (= 42 42.0)
#t
```

其他的比较函数有 `<`, `<=`, `>`, `>=`:

```
guile> (< 3 2)
#f
guile> (>= 4.5 3)
#t
```

也有一些数值计算函数:`+`, `-`, `*`, `/`, `expt`:

```
guile> (+ 1 2 3)
6
guile> (- 5.3 2)
3.3
guile> (- 5 2 1)
2
guile> (* 1 2 3)
6
guile> (/ 6 3)
2
guile> (/ 22 7)
22/7
guile> (expt 2 3)
8
guile> (expt 4 1/2)
2.0
```

其中, 对于只有一个参数的情况, `-` 函数返回参数的相反数, `/` 函数返回参数的倒数:

```
guile> (- 4)
-4
guile> (/ 4)
1/4
```

函数 `max` 和 `min` 分别返回其参数中的最大值和最小值:

```
guile> (max 1 3 4 2 3)
4
guile> (min 1 3 4 2 3)
1
```

`abs` 函数返回其参数的绝对值:

```
guile> (abs 3)
3
guile> (abs -4)
4
```

这些只是冰山一角, scheme 提供了许多和非常全面的数值计算和三角函数. 比如, `atan`, `exp` 和 `sqrt` 函数等等.

### characters(字符类型)

scheme 字符类型均是以 `#\` 开头的. 比如, `#\c` 代表 c 字符. 一些 non-graphic 字符具有描述性的名字,比如, `#\newline`、`#\tab`, 空格符可以写为 `#\`  , 但 `#\space` 更易懂.

`char?` 函数判断一个值是否属于字符类型:

```
guile> (char? #\c)
#t
guile> (char? 1)
#f
guile> (char? #\;)
#t
```

注意, 这里的分号及其后不会被注释.

字符类型有一些比较函数: `char=?`, `char<?`, `char<=?`, `char>?`, `char>=?`:

```
guile> (char=? #\a #\a)
#t
guile> (char<? #\a #\b)
#t
guile> (char>=? #\a #\b)
#f
```

可以使用 `char-ci` 来代替 char, 但这些函数比较字符时忽略字符大小写:

```
guile> (char-ci=? #\a #\A)
#t
guile> (char-ci<? #\a #\B)
#t
```

可以用 `char-downcase` 和 `char-upcase` 进行字符大小写转换:

```
guile> (char-downcase #\A)
#\a
guile> (char-upcase #\a)
#\A
```

### symbol(符号)

上面我们介绍的简单类型数据都是自求值的(self-evaluating). 比如, 如果我们在 repl 中键入这些类型的数据, 所输出的结果就是这个数据的自身:

```
guile> #t
#t
guile> 42
42
guile> #\c
#\c
```

而符号类型不是这样, 这是因为符号在 scheme 中被用作变量的标识符, 因此符号将被计算为变量的值. 尽管如此, 符号类型仍然属于简单数据类型, 在 scheme 中符号与字符、数值等等都属于合法的值.

为了使一个符号不被 scheme 认为是一个变量, 我们应该像下面用 `quote` 语句来返回这个符号:

```
guile> (quote xyz)
xyz
```

因为在 scheme 中引用(`quote`)一个符号特别常见, 对此有一个简写形式:

```
guile> 'E   ;'E 在 scheme 中与 (quote E) 等价
E
guile> (quote E)
E
```

在 guile 中, 符号的大小写是不同的. 因此 `Calorie` 和 `calorie` 是不同的符号<但是有一些其它的 scheme 实现大小写是相同的>:

```
guile> (eqv? 'Calorie 'calorie)
#f
```

我们可以利用 `define` 语句定义一个符号 `xyz` 作为一个全局变量:

```
guile> (define xyz 9)
guile> xyz
9
```

我们可以使用 `set!` 语句来改变一个变量的值:

```
guile> (set! xyz #\c)
guile> xyz
#\c
```

## 复合类型

复合数据类型由其他数据相结合构造而成.

### strings(字符串)

字符串是字符的序列, 可以用双引号括着字符序列构造字符串, 字符串是自求值的:

```
guile> "Hello, World!"
"Hello, World!"
```

`string` 函数返回其参数(字符)构造的字符串:

```
guile> (string #\h #\e #\l #\l #\o)
"hello"
```

现在让我们定义一个全局变量 `greeting`:

```
guile> (define greeting "Hello; Hello!")
```

在此注意:引号中的分号`;`并不是注释.

在一给定字符串中, 可以访问和修改任何一个字符.
`string-ref` 和 `string-set!` 函数:

```
guile> (string-ref greeting 0)
#\H
guile> (string-set! greeting 5 #\,)
guile> greeting
"Hello, Hello!"
```

`string-append` 函数可以把数个字符串组合成一个新的字符串:

```
guile> (string-append "E"
...                   "Pluribus"
...                   "Unum")
"EPluribusUnum"
```

你也可以生成一个指定长度的字符串.

```
guile> (define a-3-char-long-string (make-string 3))
```

`string?` 函数判断一个值是否为字符串.

```
guile> (string? "Hello, World")
#t
guile> (string? 123)
#f
```

有 `string`, `make-string`, `string-append` 生成的字符串是可以用 `string-set!` 函数改变的.

### vectors(向量)

向量与字符串相似, 不过其元素可以为任意类型, 不仅仅是字符. 不仅如此, 其元素也可以为向量自己, 这样就可以构造出多维向量.

下面是一种构造 5 个整数元素的向量的方法:

```
guile> (vector 0 1 2 3 4)
#(0 1 2 3 4)
```

注意:在 scheme 中, 向量的表达形式为 `#(v1 v2 v3 ...)`.

与 `make-string` 相似, `make-vector` 函数生成一个指定长度的向量:

```
guile> (define v (make-vector 5))
```

`vector-ref` 和 `vector-set!` 函数可以访问和改变一个向量中的元素.
`vector?` 判断一个值是否为向量.

### dotted pairs and lists(点对和列表)

一个点对由两个任意类型值顺序组合而成, 其中第一个元素被称为 `car`, 第二个元素被称作 `cdr`, 点对的构造函数为 `cons`.

```
guile> (cons 1 #t)
(1 . #t)
```

点对不是自求值的, 因此直接构造该数据(不通过 `cons` 函数产生)需要引用它们:

```
guile> (quote (1 . #t))
(1 . #t)
guile> '(1 . #t)
(1 . #t)
guile> (1 . #t)
ABORT: (wrong-number-of-args)
```

`car` 和 `cdr` 函数分别获取点对的第一个和第二个元素:

```
guile> (define x (cons 1 #t))
guile> (car x)
1
guile> (cdr x)
#t
```

`set-car!` 和 `set-cdr!` 函数分别改变点对的第一个和第二个元素:

```
guile> (set-car! x 2)
guile> (set-cdr! x #f)
guile> x
(2 . #f)
```

点对可以包含其他点对:

```
guile> (define y (cons (cons 1 2)
...                    3))
guile> y
((1 . 2) . 3)
```

为了分别获取值 `1` 和 `2`, 我们应该先获取该点对的第一个元素 `(1 . 2)`, 然后在获取该元素的第一个和第二个元素:

```
guile>(car (car y))
1
guile> (cdr (car y))
2
```

为此, scheme 提供了一个简写形式, `caar` 和 `cdar`:

```
guile> (caar y)
1
guile> (cdar y)
2
```

当点对多层嵌套时, scheme 有一种简写形式:

```
guile> (cons 1 (cons 2 (cons 3 (cons 4 5))))
(1 2 3 4 . 5)
```

`(1 2 3 4 . 5)` 是 `(1 . (2 . (3 . (4 . 5))))` 的简写形式, 此式最后一个 `cdr` 为 `5`.

当嵌套点对最后一个 `cdr` 为空表<被简写为 `()` >时, scheme 提供了一个更简写的形式:

```
guile> '(1 . (2 . (3 . (4 . ()))))
(1 2 3 4)
```

这种特殊类型的点对被称作为表.
scheme 中可以用 list 函数来产生一个表:

```
guile> (list 1 2 3 4)
(1 2 3 4)
```

list 中元素可以用 `list-ref` 函数来索引:

```
guile> (define y (list 1 2 3 4))
guile> (list-ref y 0)
1
```

`list-tail` 返回 `list` 中索引及以后的元素:

```
guile> (list-tail y 1)
(2 3 4)
```

函数 `pair?`, `list?` 和 `null?` 用来判断一个值是否为点对, 列表或者是空表:

```
guile> (pair? '(1 . 2))
#t
guile> (pair? '(1 2))
#t
guile> (pair? '())
#f
guile> (list? '())
#t
guile> (null? '())
#t
guile> (list? '(1 2))
#t
guile> (list? '(1 . 2))
#f
guile> (null? '(1 2))
#f
guile> (null? '(1 . 2))
#f
```

### conversions between data types(各数据类型间的变换)

scheme 提供了许多可以进行数据类型转换的函数. 
前面我们学习了通过 `char-upcase` 和 `char-downcase` 函数来进行字符的大小写转换. 
我们可以用 `char->integer` 函数来把字符类型变换为整数类型, 
同样也可以用 `integer->char` 函数把整数类型变换为字符类型:

```
guile> (char->integer #\d)
100
guile> (integer->char 100)
#\d
```

字符串可以通过 `string->list` 转换为由各个字符组成的列表:

```
guile> (string->list "hello")
(#\h #\e #\l #\l #\o)
```

还有其他的一些类似的类型转换函数: `list->string`, `vector->list` 和 `list->vector`.

数值可被转换为字符串, 同样, 字符串也可被转换为数值:

```
guile> (number->string 16)
"16"
guile> (string->number "16" 8)
14
```

其中, `string->number` 函数第二个参数是可选的, 是用来指定被转换的基数.

符号与字符串间也可以进行类型转换

```
guile> (symbol->string 'symbol)
"symbol"
guile> (string->symbol "string")
string
```

## 其他数据类型

scheme 还包含了一些其它数据类型.
过程(`procedure`)就是其中的一个. 我们已经见过了许多过程了, 例如, `display`, `+`, `cons` 等等. 
实际上，它们只是一些变量, 而这些变量被绑定到相应的过程, 这些过程并不像数值和字符具有那样可显性:

```
guile> cons
#<primitive-procedure cons>
```

迄今为止, 我们所见到的过程都是原始过程(系统过程), 由一些全局变量来引用他们. 用户还可以自定义过程.

另外一种数据类型是端口(port). 端口为输入输出提供执行通道. 端口通常会和文件和控制台相关联.

在我们的 "hello world!" 程序中，我们使用 `display` 函数向控制台输出了一个字符串.
`display` 可有两个参数, 第一个参数为要输出的值, 另一个参数就是第一个参数所要输出的端口.
在我们的程序中, `display` 未提供第二个参数, 此时, `display` 会采用默认的标准输出端口作为输出端口.
我们可以通过 `current-output-port` 函数来获取当前标准输出端口. 我们可以显式的使用 `display` 函数:

```
(display "hello, world!" (current-output-port))
```

## S-expressions(符号表达式)

s-expressions 是 symbol-expressions 的简写, 
我们前面学习过的数据类型可以被通称为 s-expressions,
因此, `42`, `#\c`, `(1 . 2)`, `#(a b c)`, `"hello"`, `(quote xyz)`, `(string->number "16")` 和 `(begin (display "hello, world!") (newline))` 都是 s-expressions.
