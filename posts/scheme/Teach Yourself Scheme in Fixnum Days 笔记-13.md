created: 2011-01-04T11:16:00+08:00
tags: [scheme]

# 第十三贴：跳转

scheme 一个显著的特性是其支持跳转<Jump>或非局部控制<nonlocal contral>。具体地说，scheme 允许程序控制流跳转到程序的任意地方，不像通过条件语句和函数调用等控制跳转那么局限。
scheme 跳转操作是通过 `call-with-current-continuation` <简写形式：call/cc>过程来实现的。下面我们将会看到如何通过该过程来实现许多惊人的控制流程<control idioms>。
 
## call-with-current-continuation
 
`call-with-current-continuation` 过程的参数是一个单参数<unary>过程，单参数过程的参数被绑定为"current continuation"。
 
在程序的任何地方，"current continuation" 都是 "the rest of the program" 的抽象。因此，下面语句中：

```
(+ 1 (call/cc
      (lambda (k)
        (+ 2 (k 3)))))
```

`call/cc` 过程的 "the rest of the program" 就是下面的有一个 `[]` 的语句：

```
(+ 1 [])
```

也就是说，这个 continuation 就是用任何值来来替 `[]` 进而与 `1` 相加的程序。

下面是 `call/cc` 过程的参数<必须为一个单参数过程>：

```
(lambda (k)
  (+ 2 (k 3)))
```

其中，`k` 就是 "the continuation"，在这个过程体中，用 `3` 调用 `k` <the continuation>，这就会使程序流程跳转到：

```
(+ 1 [])
```

<过程体 `k` 调用随后的语句不会被执行，直接跳转>,
随后用 `3` 代替 `[]`：

```
(+ 1 3)  ;结果为 4
```

上面用例子说明了用来跳出计算的 escaping continuation。但是，在 scheme 中，continuation 也可以被用来多次跳转到程序已经执行过的地方。
先睹为快，看下面程序：

```
(define r #f)
(+ 1 (call/cc
      (lambda (k)
        (set! r k)
        (+ 2 (k 3)))))
```

最后一个表达式返回 `4`，这里的 `call/cc` 调用与前面的例子中有一个不同，就是在跳转前我们把 "the continuation" 绑定到 `r` 全局变量中。现在，我们就有了一个在 `r` 变量存储的 "the continuation" 记录，该 "the continuation" 为 `(+ 1 [])`，因此，以后，我们以一个数值调用 `r` 时，都会使程序控制流跳转到程序中的 "the continuation" 的位置，并且用该数值代替 `[]` 进行以后的计算。
因此，当我们用 `5` 调用 `r` 时返回 `6`：

```
scheme@(guile-user)> (r 5)
6
```

而把 r 调用嵌入在某些环境中更能证明 continuation 跳转的作用：

```
scheme@(guile-user)> (+ 3 (r 5))
6
```

`r` 调用会将程序控制流直接跳转到 "the continuation" `(+ 1 [])` 处，再用 `5` 替换 `[]`，执行那里的代码。
 
## Escaping continuations
 
escaping continuations 是 `call/cc` 最简单的应用，但是对于跳出过程体或循环特别有用处。
让我们看一下下面的 `list-product` 过程，其以一个列表为参数，返回所有列表元素的乘积：

```
(define (list-product ls)
   (let loop ((ls ls))
      (if (null? ls)
          1
          (* (car ls)
             (loop (cdr ls))))))
```

但是上面的计算方法有一个问题，就是列表中有一个元素为 `0` 并且其后还有许多个元素，当计算到 `0` 时，结果就可以确定为 `0`，而不用在与其后的元素相乘。
此时，我们可以用 escaping continuations 重写 `list-product` 过程来避免这些无效的计算：

```
(define (list-product ls)
   (call/cc
      (lambda (exit)
         (let loop ((ls ls))
            (cond ((null? ls) 1)
                  ((= (car ls) 0) (exit 0))
                  (else (* (car s)
                           (loop (cdr ls)))))))))
```

一旦遇到 `0`，就会调用 `exit` 返回 `0`。

## Tree matching
 
一个更复杂的应用 continuation 的例子就是判断两个树的先序排列<fringe>是否相同，比如：

```
scheme@(guile-user)> (same-fringe? '(1 (2 3)) '((1 2) 3))
#t
scheme@(guile-user)> (same-fringe? '(1 2 3) '(1 (3 2)))
#f
```

一个简单的方法就是先计算出两个树的先序排列表再比较它们：

```
(define (flatten tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (flatten (car tree))
                 (flatten (cdr tree))))
        (else
         (cons (car tree)
               (flatten (cdr tree))))))
(define (same-fringe? tree1 tree2)
  (let loop ((ftree1 (flatten tree1))
             (ftree2 (flatten tree2)))
    (cond ((and (null? ftree1)
                (null? ftree2))
           #t)
          ((or (null? ftree1)
               (null? ftree2))
           #f)
          ((eqv? (car ftree1)
                 (car ftree2))
           (loop (cdr ftree1)
                 (cdr ftree2)))
          (else #f))))
```

但是这个效率比较低下。

我们可以用 `call/cc` 过程定义一个 `tree->generator` 过程来解决不必要的计算：

```
(define (tree->generator tree)
  (let ((leaf #f) (walk-tree #f))
    (define (walk-tree)
      (let loop ((tree tree))
        (cond ((null? tree) 'skip)
              ((pair? tree) (loop (car tree))
                            (loop (cdr tree)))
              (else (call/cc
                     (lambda (rest-of-tree)
                       (set! walk-tree
                             (lambda ()
                               (rest-of-tree 'resume)))
                       (leaf tree))))))
      (leaf '()))
    (define (next)
      (call/cc
       (lambda (k)
         (set! leaf k)
         (walk-tree))))
    next))
```

`tree->generator` 过程将其返回值映射到其参数树上，每次调用该返回值都会顺序返回该树先序排列所对应的元素：

```
scheme@(guile-user)> (define gen (tree->generator '((1 2) 3)))
scheme@(guile-user)> (gen)
1
scheme@(guile-user)> (gen)
2
scheme@(guile-user)> (gen)
3
scheme@(guile-user)> (gen)
()
```

利用这一特性，当我们利用 `tree->generator` 过程返回值调用来比较两个树的每一个元素时，当两个被比较的元素不相同时，我们就可以判定这两个树的先序排列不相同直接返回而不用在去理会其他未比较的元素，`tree->generator` 版的 `same-fringe?` 过程如下：

```
(define (same-fringe? tree1 tree2)
  (let ((gen1 (tree->generator tree1))
        (gen2 (tree->generator tree2)))
    (let loop ((leaf1 (gen1))
               (leaf2 (gen2)))
      (if (eqv? leaf1 leaf2)
          (if (null? leaf1)
              #t
              (loop (gen1) (gen2)))
          #f))))
```

## Coroutines

```
(define-macro (coroutine x . body)
  `(letrec ((+local-control-state (lambda (,x)
                                    ,@body))
            (resume (lambda (c v)
                      (call/cc (lambda (k)
                                 (set! +local-control-state k)
                                 (c v))))))
     (lambda (v)
       (+local-control-state v))))
 
(define (make-matcher-coroutine tree-cor-1 tree-cor-2)
  (coroutine dont-need-an-init-arg
             (let loop ()
               (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
                     (leaf2 (resume tree-cor-2 'get-a-leaf)))
                 (if (eqv? leaf1 leaf2)
                     (if (null? leaf1)
                         #t
                         (loop))
                     #f)))))
 
(define (make-leaf-gen-coroutine tree matcher-cor)
  (coroutine dont-need-an-init-arg
             (let loop ((tree tree))
               (cond ((null? tree) 'skip)
                     ((pair? tree)
                      (loop (car tree))
                      (loop (cdr tree)))
                     (else
                      (resume matcher-cor tree))))
             (resume matcher-cor '())))
 
(define (same-fringe? tree1 tree2)
  (letrec ((tree-cor-1 (make-leaf-gen-coroutine tree1
                                                (lambda (v) (matcher-cor v))))
           (tree-cor-2 (make-leaf-gen-coroutine tree2
                                                (lambda (v) (matcher-cor v))))
           (matcher-cor (make-matcher-coroutine (lambda (v) (tree-cor-1 v))
                                                (lambda (v) (tree-cor-2 v)))))
    (matcher-cor 'start-ball-rolling)))
```