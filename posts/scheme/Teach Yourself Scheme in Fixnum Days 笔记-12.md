created: 2011-01-03T10:45:00+08:00
tags: [scheme]

# 第十二贴：对象和类

类是用来描述拥有相似行为的一系列对象。由类描述的对象被称为该类的实例。类设定其实例各个槽<slot>的名字，而这些槽的值又因对象不同而不同。类也设定一系列以其实例为参数的方法<method>，槽的值可以为任何值，但是方法必须是一个过程。
 
类具有继承性，因此，一个类可以是另一个类<被称为父类>的子类，一个子类不仅有其自己的槽和方法，也继承了其父类的所有槽和方法。如果一个类和其父类拥有相同名字的槽或方法，难么该类会覆盖其父类的槽或方法。
 
## A simple object system
 
我们实现了一个简单的 scheme 对象系统。这个系统只允许单继承<一个类只能有一个父类>，如果没有父类，用 `#t` 表示无父类，而 `#t` 的父类是其自身。
 
下面的 class 结构是用来定义类的，

```
(defstruct class
   superclass slots method-names method-vector)
```

class 结构头两个域为 superclass 和 slots，分别指出父类和槽；而后两个域是 `method-names` 和 `method-vector`，用来表示类的方法。

这样我们就可以调用 `make-class` <由 defstruct class 定义> 来生成一个新的类<无父类>：

```
(define trivial-bike-class
   (make-class
    #:superclass #t
    #:slots '(frmae parts size)
    #:method-names '()
    #:method-vector #()))
```

`bike-class` 是一个非常简单的类，没有父类，没有方法，只有 `frame`、`parts`、`size` 三个槽。

复杂的类将会有父类和方法，这些就需要许多的初始化工作，为了简化初始化代码，我们可以写一个宏 `defclass`：

```
(define-macro (defclass klass superclass slots . methods)
  `(define ,klass 
     (defclass-proc
       ,superclass
       (list ,@(map (lambda (slot) `',slot) slots))
       (list ,@(map (lambda (method) `',(car method)) methods))
       (vector ,@(map (lambda (method) `,(cadr method)) methods)))))
```

其中的 `defclass-proc` 过程见后文。
这样我们就可以用 `defclass` 宏来定义一个类：

```
(defclass bike-class
  #t
  (frame size parts chain tires)
  (check-fit (lambda (self inseam)
               (let ((bike-size (slot-ref self 'size))
                     (ideal-size (* inseam 3/5)))
                 (let ((diff (- bike-size ideal-size)))
                   (cond ((<= -1 diff 1) 'perfect-fit)
                         ((<= -2 diff 2) 'fits-well)
                         ((< diff -2) 'too-small)
                         ((< diff 2) 'too-big)))))))
```

我们可以定义一个 `make-instance` 过程产生一个类的实例<基于类定义的向量>：

```
(define (make-instance klass . slots-name-value)
  (let* ((slots (class.slots klass))
         (n (length slots))
         (instance (make-vector (+ n 1))))
    (vector-set! instance 0 klass)
    (let loop ((slots-name-value slots-name-value))
      (if (null? slots-name-value)
          instance
          (let ((i (list-position (keyword->symbol (car slots-name-value))
                                  slots)))
            (vector-set! instance (+ i 1) (cadr slots-name-value))
            (loop (cddr slots-name-value)))))))
```

其中的 list-position 过程见后文。

实例的格式特别简单：
实例向量的首个元素为其类型；
实例向量的其余元素为槽值<slot values>。
`make-instance` 的第一个参数为其类型，其后为一系列的序对<对应槽名和槽值>：

下面是用 `make-instance` 过程定义 `bike-class` 的一个实例：

```
(define my-bike
  (make-instance bike-class
                 #:frame 'titanium
                 #:size 21
                 #:parts 'ultegra
                 #:chain 'sachs
                 #:tires 'continental))
```

我们也可以用 `defclass` 宏定义 `bike-class` 类的一个子类：

```
(defclass mtn-bike-class
  bike-class
  (suspension)
  (check-fit (lambda (self inseam)
               (let ((bike-size (slot-ref self 'size))
                     (ideal-size (- (* inseam 3/5) 2)))
                 (let ((diff (- bike-size ideal-size)))
                   (cond ((<= -2 diff 2) 'perfect-fit)
                         ((<= -4 diff 4) 'fits-well)
                         ((< diff -4) 'too-small)
                         ((> diff 4) 'too-big)))))))
```

我们可以定义一个 `class-of` 过程来得到一个实例的类型：

```
(define (class-of x)
  (if (vector? x)
      (let ((n (vector-length x)))
        (if (>= n 1) 
            (let ((c (vector-ref x 0)))
              (if (class? c) c #t))
            #t))
      #t))
```

一个不是由 class 类产生的 scheme 对象的 `class-of` 被定义为 `#t` <无类型>。

过程 `slot-ref` 和 `slot-set!` 分别读取和改变类实例的槽值：

```
(define (slot-ref instance slot)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (vector-ref instance (+ slot-index 1))))
 
(define (slot-set! instance slot new-val)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (vector-set! instance (+ slot-index 1) new-val)))
```

过程 `send` 是从实例的类中 `method-vector` 域寻找方法来对其实例进行操作：

```
(define (send method instance . args)
  (let ((proc (let loop ((klass (class-of instance)))
                (if (eqv? klass #t)
                    (error 'send)
                    (let ((i (list-position method
                                            (class.method-names klass))))
                      (if i
                          (vector-ref (class.method-vector klass) i)
                          (loop (class.superclass klass))))))))
    (apply proc instance args)))
```

这样我们可以调用 `send` 来完成类方法的操作：

```
(send 'check-fit my-bike 32)
```

以下是实现单继承类的代码：

```
(define (list-position o l)
  (let loop ((i 0) (l l))
    (if (null? l)
        #f
        (if (eqv? (car l) o)
            i
            (loop (+ i 1) (cdr l))))))
 
(define (delete-duplicates s)
  (if (null? s)
      s
      (let ((a (car s))
            (d (cdr s)))
        (if (memv a d)
            (delete-duplicates d)
            (cons a (delete-duplicates d))))))
 
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
 
(defstruct class
  superclass slots method-names method-vector)
 
(define-macro (defclass klass superclass slots . methods)
  `(define ,klass 
     (defclass-proc
       ,superclass
       (list ,@(map (lambda (slot) `',slot) slots))
       (list ,@(map (lambda (method) `',(car method)) methods))
       (vector ,@(map (lambda (method) `,(cadr method)) methods)))))
   
(define (defclass-proc superclass slots method-names method-vector)
  (make-class
   #:superclass superclass
   #:slots (let ((superclass-slots (if (eqv? superclass #t)
                                       '()
                                       (class.slots superclass))))
             (delete-duplicates (append slots superclass-slots)))
   #:method-names method-names
   #:method-vector method-vector))
 
(define (make-instance klass . slots-name-value)
  (let* ((slots (class.slots klass))
         (n (length slots))
         (instance (make-vector (+ n 1))))
    (vector-set! instance 0 klass)
    (let loop ((slots-name-value slots-name-value))
      (if (null? slots-name-value)
          instance
          (let ((i (list-position (keyword->symbol (car slots-name-value))
                                  slots)))
            (vector-set! instance (+ i 1) (cadr slots-name-value))
            (loop (cddr slots-name-value)))))))
 
(define (class-of x)
  (if (vector? x)
      (let ((n (vector-length x)))
        (if (>= n 1) 
            (let ((c (vector-ref x 0)))
              (if (class? c) c #t))
            #t))
      #t))
 
(define (slot-ref instance slot)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (vector-ref instance (+ slot-index 1))))
 
(define (slot-set! instance slot new-val)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (vector-set! instance (+ slot-index 1) new-val)))
 
(define (send method instance . args)
  (let ((proc (let loop ((klass (class-of instance)))
                (if (eqv? klass #t)
                    (error 'send)
                    (let ((i (list-position method
                                            (class.method-names klass))))
                      (if i
                          (vector-ref (class.method-vector klass) i)
                          (loop (class.superclass klass))))))))
    (apply proc instance args)))
```

## Classes are instances too


到此，我们已经介绍了一个简单的对象系统，但是聪明的读者也许会看出：其实类本身也是某类<metaclass>的实例。因为所有的类都具有相同的特性：都具有槽、父类、方法名字列表、方法向量。每个类看起来都有 `make-instance` 方法。这些这本身就符合“定义一个类来设定所有一些对象<所有类>共有的特性”。
 
下面我们将弃用上面定义的 class 结构，重新定义一个 class 向量<metaclass>来设定所有类的特性，同时 class 本身也是其自己的实例：

```
(define class
  (vector 'value-of-class           ;类型<随后设定>
          #t                        ;父类
          (list 'superclass         ;槽
                'slots
                'method-names
                'method-vector)
          '(make-instance)          ;方法名     
          (vector make-instance)))  ;方法过程
(vector-set! class 0 class)         ;设定类型
```

这样我们就不能使用下面基于 class 结构的过程了：

```
(class? x)(
(class.superclass c)
(class.slots c)
(class.method-names c)
(class.method-vector c)
(make-class ...)
```

需要用下面的方式来达到同样的目的：

```
(and (vector? x)
     (eqv? (vector-ref x 0)
           class))
(vector-ref c 1)
(vector-ref c 2)
(vector-ref c 3)
(vector-ref c 4)
(send 'make-instance class ...)
```

## Multiple inheritance

通过给 class 加一个 `precedence-list` 槽可以很容易实现多继承：

```
(define class
  (vector 'value-of-class
          '()
          (list 'precedence-list
                'slots
                'method-names
                'method-vector)
          '(make-instance)
          (vector make-instance)))
(vector-set! class 0 class)
```

实现多继承的代码如下：

```
(define (list-position o l)
  (let loop ((i 0) (l l))
    (if (null? l)
        #f
        (if (eqv? (car l) o)
            i
            (loop (+ i 1) (cdr l))))))
 
(define (delete-duplicates s)
  (if (null? s)
      s
      (let ((a (car s))
            (d (cdr s)))
        (if (memv a d)
            (delete-duplicates d)
            (cons a (delete-duplicates d))))))
 
(define (append-map f ls)
  (let loop ((ls ls))
    (if (null? ls)
        '()
        (append (f (car ls))
                (loop (cdr ls))))))
 
(define (class? x)
  (and (vector? x)
       (eqv? (vector-ref x 0)
             class)))
 
(define (class.precedence-list klass)
  (if (class? klass)
      (vector-ref klass 1)
      (error 'superclasses-of)))
 
(define (class.slots klass)
  (if (class? klass)
      (vector-ref klass 2)
      (error 'class-slots)))
 
(define (class.method-names klass)
  (if (class? klass)
      (vector-ref klass 3)
      (error 'class-method-names)))
 
(define (class.method-vector klass)
  (if (class? klass)
      (vector-ref klass 4)
      (error 'class-method-vector)))
 
(define (class-of instance)
  (if (vector? instance)
      (let ((n (vector-length instance)))
        (if (>= n 1)
            (let ((klass (vector-ref instance 0)))
              (if (class? klass)
                  klass
                  #t))
            #t))
      #t))
 
(define (make-instance klass . slots-name-value)
  (let* ((slots (class.slots klass))
         (n (length slots))
         (instance (make-vector (+ n 1))))
    (vector-set! instance 0 klass)
    (let loop ((slots-name-value slots-name-value))
      (if (null? slots-name-value)
          instance
          (let ((i (list-position (keyword->symbol (car slots-name-value))
                                  slots)))
            (if i
                (vector-set! instance (+ i 1) (cadr slots-name-value))
                (error 'make-instance))
            (loop (cddr slots-name-value)))))))
 
(define class
  (vector 'value-of-class
          '()
          (list 'precedence-list
                'slots
                'method-names
                'method-vector)
          '(make-instance)
          (vector make-instance)))
(vector-set! class 0 class)
 
(define-macro (defclass klass direct-superclass-list slots . methods)
  `(define ,klass 
     (defclass-proc
       (list ,@(map (lambda (superclass) `,superclass) direct-superclass-list))
       (list ,@(map (lambda (slot) `',slot) slots))
       (list ,@(map (lambda (method) `',(car method)) methods))
       (vector ,@(map (lambda (method) `,(cadr method)) methods)))))
 
(define (defclass-proc direct-superclass-list slots method-names method-vector)
  (let ((precedence-list (delete-duplicates
                          (append direct-superclass-list
                                  (append-map (lambda (c) (class.precedence-list c))
                                              direct-superclass-list)))))
    (send 'make-instance
          class
          #:precedence-list precedence-list
          #:slots (delete-duplicates
                   (append slots (append-map
                                  (lambda (c) (class.slots c))
                                  precedence-list)))
          #:method-names method-names
          #:method-vector method-vector)))
 
(define (send method-name instance . args)
  (let ((proc (let ((klass (class-of instance)))
                (if (eqv? klass #t)
                    (error 'send)
                    (let loop ((klass klass)
                               (precedence-list (class.precedence-list klass)))
                      (let ((i (list-position method-name
                                              (class.method-names klass))))
                        (cond (i
                               (vector-ref (class.method-vector klass)
                                           i))
                              ((null? precedence-list)
                               (error 'send))
                              (else (loop (car precedence-list)
                                          (cdr precedence-list))))))))))
    (apply proc instance args)))
 
(define (slot-ref instance slot)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (if slot-index
        (vector-ref instance (+ slot-index 1))
        (error 'slot-ref))))
 
(define (slot-set! instance slot new-val)
  (let* ((klass (class-of instance))
         (slot-index (list-position slot (class.slots klass))))
    (if slot-index
        (vector-set! instance (+ slot-index 1) new-val)
        (error 'slot-set!))))
 
(defclass bike-class
  ()
  (frame size parts chain tires)
  (check-fit (lambda (self inseam)
               (let ((bike-size (slot-ref self 'size))
                     (ideal-size (* inseam 3/5)))
                 (let ((diff (- bike-size ideal-size)))
                   (cond ((<= -1 diff 1) 'perfect-fit)
                         ((<= -2 diff 2) 'fits-well)
                         ((< diff -2) 'too-small)
                         ((< diff 2) 'too-big)))))))
 
(defclass mtn-bike-class
  (bike-class)
  (suspension)
  (check-fit (lambda (self inseam)
               (let ((bike-size (slot-ref self 'size))
                     (ideal-size (- (* inseam 3/5) 2)))
                 (let ((diff (- bike-size ideal-size)))
                   (cond ((<= -2 diff 2) 'perfect-fit)
                         ((<= -4 diff 4) 'fits-well)
                         ((< diff -4) 'too-small)
                         ((> diff 4) 'too-big)))))))
 
(define my-bike
  (send 'make-instance bike-class
        #:frame 'titanium
        #:size 21
        #:parts 'ultegra
        #:chain 'sachs
        #:tires 'continental))
```
