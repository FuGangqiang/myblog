created: 2011-01-05T19:23:00+08:00
tags: [scheme]

# 第十四贴：不确定性

注：本文很大一部分来自王垠的主页
 
尽管 McCarthy 的 `amb` 操作符不存在于 Lisp 中，但是它跟 Lisp 一样古老。
`amb` 接受一些参数，它会从这些参数里“不确定”的选一个出来。选择的标准就是让整个程序能够得到“有效的结果”。
使用它，我们可以轻而易举的写出需要大量回溯才能解决的问题。它可以被作为一种通用的回溯机制。
 
下面我们就在 scheme 中实现 `amb` 操作符。
 
## Description of amb
 
在 SICP<Structure and Interpretation of Computer Programs> 中可以找到有关 `amb` 操作符的介绍。
`amb` 会从它的参数里选出一个来让整个程序得到“有效的结果”。例如：

```
(amb 1 2)
```

上面的表达式将会返回 `1` 或者 `2` 。
<返回 1 或者 2 都会让程序继续运行>

“有效的结果”这个概念很模糊，什么叫做“有效的结果”？
为了定义“有效的结果”，我们首先定义一下“无效的结果”，或者“失败的结果”：

```
(amb)
```

没有参数的 `amb` 被定义为是一个“无效的结果”。因此，

```
(amb 1 (amb))
```

和

```
(amb (amb) 1)
```

都会返回 `1`,因为 `amb` 会从其参数中选择一个并将其返回，还要使程序有效的运行下去，`amb` 必须返回其参数 `1`，若返回其参数 `(amb)`，将会得到无效的结果而使程序无效。
同样，下面的表达式：

```
(if (amb #f #t)
    1
    (amb))
```

也返回 `1`。
上面的 `(amb #f #t)` 可以返回 `#f` 和 `#t` 中的任一个，但是如果返回 `#f`，`if` 语句将会返回 `(amb)` 以使程序无效，因此 `(amb #f #t)` 会从其参数中选择 `#t`，以使 `if` 语句返回 `1`。
 
## Implementing amb in scheme
 
### 初始化

`amb:fail` 是最近一个失败的分支设置的函数。
如果执行没有参数的 `(amb)` 就会转到这个 `amb:fail`.
 
这里我们把 `amb:fail` 被初始化为打印 "amb tree exhausted"。

```
(define amb:fail #f)
 
(define (amb:init-fail)
  (set! amb:fail (lambda ()
                   (error "amb tree exhausted"))))
(amb:init-fail)
```

### amb 实现

```
(define-macro (amb . alts...)
  `(let ((+amb:prev-fail amb:fail))
     (call/cc (lambda (+choose)
                 
                ,@(map (lambda (alt)
                         `(call/cc (lambda (+jump)
                                     (set! amb:fail (lambda ()
                                                      (set! amb:fail +amb:prev-fail)
                                                      (+jump 'next)))
                                     (+choose ,alt))))
                       alts...)
                 
                (+amb:prev-fail)))))
```

## Using amb in scheme

在 1 到 10 之间选择一个数，我们可以用

```
(amb 1 2 3 4 5 6 7 8 9 10)
```

可以肯定的是，单独作为程序，上面的表达式将会返回 1，但是如果嵌入到其他 scheme 语句中，它就会从 1 到 10 中选择一个数，来保证程序有效。
 
以上只是从 1 到 10 之间选择数值，我们可以一一写出它们，但是如果从 1 到 1000 之间选择数值一一写出就比较麻烦了，因此，我们定义一个 number-between 过程，
其提供了一个更抽象的方法来产生一个数据序列：

```
(define (number-between lo hi)
  (let loop ((i lo))
    (if (> i hi)
        (amb)
        (amb i (loop (+ i 1))))))
```

因此：

```
(number-between 1 10)
```

也就相当于：

```
(amb 1 2 3 4 5 6 7 8 9 10)
```

如果是 `(number-between 1 1000)` 就可以省去打印许多数字了。
 
我们可以用下面的 `amb:assert` 来插入一个断言，以使程序的表达更加清晰明确：

```
(define (amb:assert pred)
  (if (not pred)
      (amb)))
```

下面是一个应用 `amb:assert` 过程来产生一个小于 `hi` 的偶数：

```
(define (gen-even hi)
  (let ((i (number-between 2 hi)))
    (amb:assert (even? i))
    i))
```

这个过程相当的简单，只是如果单独调用它 `(gen-even 10)`，它只是返回 `2`。
如果我们想要所有的值，一种解决的办法是在其后调用 `amb`：

```
scheme@(guile-user)> (gen-even 8)
2
scheme@(guile-user)> (amb)
4
scheme@(guile-user)> (amb)
6
scheme@(guile-user)> (amb)
8
scheme@(guile-user)> (amb)
ERROR: amb tree exhausted
```

`amb` 每次只返回一个结果，所以如果想得到所有可以使得程序有效执行的结果，你需要多次调用 `amb`，为了一次性得到所有结果，我们定义了 `bag-of` 宏：

```
(define-macro (bag-of e)
  `(let ((+amb:prev-fail amb:fail)
         (+results '()))
     (if (call/cc (lambda (+k)
                    (set! amb:fail (lambda ()
                                     (+k #f)))
                    (let ((+v ,e))
                      (set! +results (cons +v +results))
                      (+k #t))))
         (amb:fail))
     (set! amb:fail +amb:prev-fail)
     (reverse! +results)))
```

这样我们就可以用

```
scheme@(guile-user)> (bag-of (gen-even 10))
(2 4 6 8 10)
```

来得到所有小于 10 的偶数了。
 
## Logic puzzles
 
带有回溯的深度优先搜索能力的 `amb` 特别适用于解决逻辑难题。
 
### The Kalotan puzzle

> 有一个部落叫 Kalotan，这里的人有一个很奇怪的特点，那就是男性从来只说真话，女性从来不会连续说两句真话，也不会连续说两句假话。
> 有一天，一个人类学家来到这个部落，遇到一对(异性)夫妇和他们的小孩 Kibi。人类学家问 Kibi：“你是男孩还是女孩？”
> Kibi 说了一句 Kalotan 语，人类学家听不懂，于是转向 Kibi 的父母询问答案(他们会说英语)。于是其中一个(parent1)对他说： “Kibi 说他是男孩。” 另一个(parent2)对他说：“Kibi 是个女孩。 Kibi 撒谎了。”
> 请你判断 parent1, parent2 和 Kibi 各自的性别。
 
解决代码如下：

```
(define (distinct? . ls)
  (let loop ((ls (car ls)))
    (let ((first (car ls))
          (rest (cdr ls)))
      (cond ((null? rest) #t)
            ((member first rest) #f)
            (else (loop rest))))))
 
(define (solve-kalotan-puzzle)
  (let ((parent1 (amb 'm 'f))
        (parent2 (amb 'm 'f))
        (kibi (amb 'm 'f))
        (kibi-self-desc (amb 'm 'f))
        (kibi-lied? (amb #t #f)))
    (amb:assert (distinct? (list parent1
                                 parent2)))
    (amb:assert (if (eqv? kibi 'm)
                    (not kibi-lied?)))
    (amb:assert (if kibi-lied?
                    (not (eqv? (and (eqv? kibi-self-desc 'm)
                                    (eqv? kibi 'f))
                               (and (eqv? kibi-self-desc 'f)
                                    (eqv? kibi 'm))))))
    (amb:assert (if (not kibi-lied?)
                    (not (eqv? (and (eqv? kibi-self-desc 'm)
                                    (eqv? kibi 'm))
                               (and (eqv? kibi-self-desc 'f)
                                    (eqv? kibi 'f))))))
    (amb:assert (if (eqv? parent1 'm)
                    (and (eqv? kibi-self-desc 'm)
                         (not (eqv? (and (eqv? kibi 'f)
                                         (eqv? kibi-lied? #f))
                                    (and (eqv? kibi 'm)
                                         (eqv? kibi-lied? #t)))))))
    (amb:assert (if (eqv? parent1 'f)
                    (and (eqv? kibi 'f)
                         (eqv? kibi-lied? #t))))
    (list parent1 parent2 kibi)))
```

这样调用 `solve-kalotan-puzzle` 过程就可以得到答案了。

### Map coloring

用四种颜色为欧洲地图着色，但是相邻的国家颜色不同，问怎么来着色？
 
解决代码如下：

```
(define (choose-color)
  (amb 'red 'yellow 'blue 'white))
 
(define (color-europe)
 
  ;choose colors for each country
  (let ((p (choose-color)) ;Portugal
        (e (choose-color)) ;Spain
        (f (choose-color)) ;France
        (b (choose-color)) ;Belgium
        (h (choose-color)) ;Holland
        (g (choose-color)) ;Germany
        (l (choose-color)) ;Luxemb
        (i (choose-color)) ;Italy
        (s (choose-color)) ;Switz
        (a (choose-color)) ;Austria
        )
     
    ;construct the adjacency list for
    ;each country: the 1st element is
    ;the name of the country; the 2nd
    ;element is its color; the 3rd
    ;element is the list of its
    ;neighbors' colors
    (let ((portugal
           (list 'portugal p
                 (list e)))
          (spain
           (list 'spain e
                 (list f p)))
          (france
           (list 'france f
                 (list e i s b g l)))
          (belgium
           (list 'belgium b
                 (list f h l g)))
          (holland
           (list 'holland h
                 (list b g)))
          (germany
           (list 'germany g
                 (list f a s h b l)))
          (luxembourg
           (list 'luxembourg l
                 (list f b g)))
          (italy
           (list 'italy i
                 (list f a s)))
          (switzerland
           (list 'switzerland s
                 (list f i a g)))
          (austria
           (list 'austria a
                 (list i s g))))
      (let ((countries
             (list portugal spain
                   france belgium
                   holland germany
                   luxembourg
                   italy switzerland
                   austria)))
         
        ;the color of a country
        ;should not be the color of
        ;any of its neighbors
        (for-each
         (lambda (c)
           (amb:assert (not (memq (cadr c)
                                  (caddr c)))))
         countries)
         
        ;output the color
        ;assignment
        (for-each
         (lambda (c)
           (display (car c))
           (display " ")
           (display (cadr c))
           (newline))
         countries)))))
```

调用 `color-europe` 过程就会得到一种着色方案，但是如果想要得到所有的着色方案可以用 `(bag-of (color-europe))`.