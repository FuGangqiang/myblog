created: 2010-12-26T12:04:00+08:00
tags: [scheme]

# 第十贴：关联列表<association>和表<tables>

关联列表是一种特殊格式的 scheme 列表，该列表每个元素都是一个点对，其中每个点对的 `car` 称为键值<key>，`cdr` 称为键值所关联的值。

下面就是一个关联列表：

```
((a . 1) (b . 2) (c . 3))
```

`(assv k al)` 过程调用返回关联列表 `al` 中键值为 `k` 的点对，`al` 中的键值与 `k` 的比较过程为 `eqv?`。
但是，我们通常需要用不同的比较函数来比较 `al` 中的键值和 `k`。
比如，键值和 `k` 比较时忽略大小写，`eqv?` 过程就不起作用了，进而不能用 `assv` 过程了。

我们可以定义一个称为 table 的结构来实现我们的需求。
table 由两个域组成，一个为关联列表 `alist`，一个为用户自定义的键值比较函数 `equ`，定义如下：

```
(defstruct table (equ eqv?) (alist '()))
```

其中：默认比较函数为 `eqv?`，`alist` 的初始值为空表。

我们使用 `table-get` 过程来得到一个给定键的关联值，其参数为一个 `table`、`key` 和一个可选参数，如果在 `table.alist` 中没发现 `key` 所对应的点对就返回可选参数。

```
(define (table-get tbl k . d)
   (let ((c (lassoc k (table.alist tbl) (table.equ tbl))))
      (cond (c (cdr c))
            ((pair? d) (car d)))))
```

其中 `table-get` 所调用的 `lassoc` 过程定义如下：

```
(define (lassoc k al equ?)
   (let loop ((al al))
      (if (null? al)
          #f
          (let ((c (car al)))
             (if (equ? (car c) k)
                 c
                (loop (cdr al)))))))
```

我们使用 `table-put!` 过程来更新给定 `table` 相应键的值：

```
(define (table-put! tbl k v)
   (let* ((al (table.alist tbl))
          (c (lassoc k al (table.equ tbl))))
      (if c
          (set-cdr! c v)
          (set!table.alist tbl (cons (cons k v)
                                     al)))))
```

`table-for-each` 过程对 `table` 中的每个点对都进行给定的操作：

```
(define (table-for-each tbl p)
   (for-each (lambda (c)
                (p (car c)
                   (cdr c)))
             (table.alist tbl)))
```
