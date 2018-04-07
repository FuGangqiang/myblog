created: 2010-12-25T09:12:00+08:00
tags: [scheme]

# 第九贴：结构

本篇你将体会到 lisp/scheme 宏的强大之处
 
结构就是被自然组织在一起的数据，可以使用 scheme 的复合数据类型<例如向量、列表>来表示结构。
 
下面，我们就通过刚学习到的宏来自定义一个树结构。这个树结构包含有 `height`，`girth`，`age`，`leaf-shape`和`leaf-color`五个域名。
 
这个结构可以被表示为一个长度为 `5` 的向量，而那些域可以通过 `vector-ref` 和 `vector-set!` 过程来访问和修改。
但是每时每刻都记着每一域名所对应的索引是很容易出错的，因此我们写一个宏 `defstruct` 来定义一个数据结构<基于向量>，并且同时定义一些实例化、访问和修改该数据结构的过程。
 
这个 tree 结构可以被这样定义：

```
(defstruct tree height girth age leaf-shape leaf-color)
```

以上定义了一个构造过程 `make-tree`，每个域的访问过程为 `tree.height`，`tree.girth` 等等，每个域的修改过程为 `set!tree.heitht`，`set!tree.girth` 等等。

其中构造过程可以这样使用：

```
(define coconut
   (make-tree #:height 30
              #:leaf-shape 'frond
              #:age 5))
```

构造过程参数是一个双数，由 key-value 对组成，如果有的域名没有在构造过程中被初始化，其值就是`*unspecified*`。
 
访问过程可以这样被使用：

```
(tree.height coconut)        ;会得到 30
(tree.leaf-shape coconut)    ;会得到 frond
(tree.girth coconut)         ;会得到 *unspecified* <guile 默认不显示任何内容>
```

修改过程可以这样被使用：

```
(set!tree.height coconut 40)
(set!tree.girth coconut 10)
```

此后，我们再次访问 `coconut` 相关的域就会得到新的值：

```
(tree.heitht coconut)       ;会得到 40
(tree.girth coconut)        ;会得到 10
```

## Default initializations
 
我们也可以像这样在定义结构时初始化一些指定的域名的值：

```
(defstruct tree heitht girth age
                (leaf-shape 'frond)
                (leaf-color 'green))
```

这样我们像下面定义的 `palm` 实例就会使其 `leaf-shape` 和 `leaf-color` 的值分别为 `'frond` 和 `'green`。

```
(define palm (make-tree #:height 60))
(tree.height palm)                     ;会得到 60
(tree.lefa-shape palm)                 ;会得到 frond
(tree.leaf-color palm)                 ;会得到 green
```

## defstruct defined

上面只是介绍了我们想要的功能，下面才是这些功能的实现：

```
;;;注：本例是在 guile 中运行，如果为其他 scheme 实现可参考原文
 
;;; list-position 在 defstruct 宏定义中会被调用
(define (list-position o l)
  (let loop ((i 0) (l l))
    (if (null? l)
        #f
        (if (eqv? (car l) o)
            i
            (loop (+ i 1) (cdr l))))))
 
;;; defstruct 定义
(define-macro (defstruct s . ff)
  (let ((s-s (symbol->string s))
        (n (length ff)))
    (let* ((n+1 (+ n 1))
           (vv (make-vector n+1)))
      (let loop ((i 1) (ff ff))
        (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i (if (pair? f)
                                    (cadr f)
                                    '*unspecified*))
              (loop (+ i 1) (cdr ff)))))
      (let ((ff (map (lambda (f) (if (pair? f)
                                     (car f)
                                     f))
                     ff)))
        `(begin
           (define ,(string->symbol
                     (string-append "make-" s-s))
             (lambda fvfv
               (let ((st (make-vector ,n+1))
                     (ff ',ff))
                 (vector-set! st 0 ',s)
                 ,@(let loop ((i 1) (r '()))
                     (if (>= i n+1)
                         r
                         (loop (+ i 1)
                               (cons `(vector-set! st ,i ,(vector-ref vv i))
                                     r))))
                 (let loop ((fvfv fvfv))
                   (if (not (null? fvfv))
                       (begin (vector-set! st
                                           (+ (list-position (keyword->symbol (car fvfv))
                                                             ff)
                                              1)
                                           (cadr fvfv))
                              (loop (cddr fvfv)))))
                 st)))
           ,@(let loop ((i 1) (procs '()))
               (if (>= i n+1)
                   procs
                   (loop (+ i 1)
                         (let ((f (symbol->string
                                   (list-ref ff (- i 1)))))
                           (cons
                            `(define ,(string->symbol
                                       (string-append s-s "." f))
                               (lambda (x)
                                 (vector-ref x ,i)))
                            (cons
                             `(define ,(string->symbol
                                        (string-append "set!" s-s "." f))
                                (lambda (x v)
                                  (vector-set! x ,i v)))
                             procs))))))
           (define ,(string->symbol (string-append s-s "?"))
             (lambda (x)
               (and (vector? x)
                    (eqv? (vector-ref x 0)
                          ',s)))))))))
```
