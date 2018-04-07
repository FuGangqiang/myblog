created: 2010-12-02T14:25:00+08:00
tags: [scheme]


GOOPS(Guile Object-Oriented Programming System),
Guile 的一个面向对象系统扩展模块, 类似于 CLOS(Common Lisp Object System), 适用于 scheme 环境.

GOOPS文档:[http://www.gnu.org/software/guile/docs/goops/index.html#Top](http://www.gnu.org/software/guile/docs/goops/index.html#Top)

加载 goops 模块:

```
guile> (use-modules (oop goops))
```

让我们先看几个例子:

1. 重载 `+` 函数, 以使字符串可以相加, 结果为字符串的顺序组合:
    ```
    guile> (define-method (+ (x <string>) (y <string>))
    ...        (string-append x y))
    guile> (+ "abc" "def")
    "abcdef"
    guile> (+ "abc" "123" "def")
    "abc123def"
    ```
2. 类型定义，定义了一个数学中二维向量, 其中有两个属性: x 和 y：
    ```
    guile> (define-class <2d-vector> ()
    ...        (x #:init-value 0 #:accessor x #:init-keyword #:x)
    ...        (y #:init-value 0 #:accessor y #:init-keyword #:y))
    ```
3. 对象生成，可以用 make 来创建一个二维向量:
    ```
    guile> (define v1 (make <2d-vector>))               ; x, y 被设为默认值 (0, 0)
    guile> (define v2 (make <2d-vector> #:x 1 #:y 2))   ; x, y 被设为 (1, 2)
    guile> v1
    #<<2d-vector> b75fe9e0>
    guile> v2
    #<<2d-vector> b75fde80>
    ```
4. 对象属性，可以访问和改变对象的 x 和 y 的值:
    ```
    guile> (x v1)
    0
    guile> (y v1)
    0
    guile> (x v2)
    1
    guile> (y v2)
    2
    guile> (set! (x v1) 3)
    guile> (set! (y v1) 4)
    guile> (x v1)
    3
    guile> (y v1)
    4
    ```
5. 重载函数，可以重载 +, 以使向量相加:
    ```
    guile> (define-method (+ (v1 <2d-vector>) (v2 <2d-vector>))
    ...        (make <2d-vector>
    ...              #:x (+ (x v1) (x v2))
    ...              #:y (+ (y v1) (y v2))))
    
    guile> (define v3 (+ v1 v2))
    guile> (x v3)
    4
    guile> (y v3)
    6
    ```
6. 判断类型，可以得到一个对象的类型, 或者判断一个对象是否属于某个类型:
    ```
    guile> (class-of v1)
    #<<class> <2d-vector> b75fdc30>
    guile> (class-of 1)
    #<<class> <integer> b760ec30>
    guile> (is-a? v1 <2d-vector>)
    #t
    ```
