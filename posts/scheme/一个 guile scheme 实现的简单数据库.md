created: 2011-01-21T18:44:00+08:00
tags: [scheme]

本文是模仿 Practical common lisp 中第三章<Practical:A Simple Database>中的程序而成的。

在此，我用 scheme 的传统宏实现了一个类似的简单数据库。
 
代码如下：

```
(use-modules (ice-9 format))
 
(define-macro (create-table name columns)
  (let ((+name-s (symbol->string name)))
    `(begin
       (define ,(string->symbol (string-append "*" +name-s "-table*"))
         '())
 
       (define ,(string->symbol (string-append "make-" +name-s "-record"))
         (lambda +cols
           (map cons ,columns +cols)))
 
       (define ,(string->symbol (string-append "add-" +name-s "-record"))
         (lambda (+record)
           (set! ,(string->symbol (string-append "*" +name-s "-table*"))
                 (cons +record
                       ,(string->symbol (string-append "*" +name-s "-table*"))))))
 
       (define ,(string->symbol (string-append "dump-" +name-s "-table"))
         (lambda ()
           (format #t "~{~{~a~^~%~}~^~2%~}"
                   ,(string->symbol (string-append "*" +name-s "-table*")))))
 
       (define ,(string->symbol (string-append "save-" +name-s "-table"))
         (lambda (+filename)
           (with-output-to-file +filename
             (lambda ()
               (write ,(string->symbol (string-append "*" +name-s "-table*")))))))
 
       (define ,(string->symbol (string-append "load-" +name-s "-table"))
         (lambda (+filename)
           (with-input-from-file +filename
             (lambda ()
               (set! ,(string->symbol (string-append "*" +name-s "-table*"))
                     (read))))))
 
       (define ,(string->symbol (string-append "where-" +name-s))
         (lambda +pairs
           (lambda (+cd)
             (let +loop ((+ls +pairs))
               (if (null? +ls)
                   #t
                   (let ((+key (car +ls))
                         (+value (cadr +ls)))
                     (if (equal? +value
                                 (cdr (assq +key +cd)))
                         (+loop (cddr +ls))
                         #f)))))))
 
       (define ,(string->symbol (string-append "select-" +name-s))
         (lambda (+select-fn)
           (filter +select-fn ,(string->symbol (string-append "*" +name-s "-table*")))))
 
       (define ,(string->symbol (string-append "update-" +name-s))
         (lambda (+selector-fn . +pairs)
           (for-each (lambda (+cd)
                       (if (+selector-fn +cd)
                           (let loop ((+ls +pairs))
                             (if (not (null? +ls))
                                 (let ((+key (car +ls))
                                       (+value (cadr +ls)))
                                   (let ((+temp (assq +key +cd)))
                                     (if (not (pair? +temp))
                                         (error "Wrong arguments in update function!")
                                         (set-cdr! +temp +value))
                                     (loop (cddr +ls))))))))
                     ,(string->symbol (string-append "*" +name-s "-table*")))))
 
       )))
```

运行以上代码后，我们就有了一个 create-table 宏：
如果像下面运行该宏：

```
(create-table cd '(title artist rating ripped))
```

就会建立了一个 cd 数据库，即是定义了以下变量和过程：

* `*cd-table*`
* `make-cd-record`
* `add-cd-record`
* `dump-cd-table`
* `save-cd-table`
* `load-cd-table`
* `where-cd`
* `select-cd`
* `update-cd`

应用如下：

```
scheme@(guile-user)> (create-table cd '(title artist rating ripped))
scheme@(guile-user)> (add-cd-record (make-cd-record "Roses" "Kathy Mattea" 7 #t))
scheme@(guile-user)> (add-cd-record (make-cd-record "Fly" "Dixie Chicks" 8 #t))
scheme@(guile-user)> (add-cd-record (make-cd-record "Home" "Dixie Chicks" 9 #t))
scheme@(guile-user)> (dump-cd-table)
(title . Home)
(artist . Dixie Chicks)
(rating . 9)
(ripped . #t)
 
(title . Fly)
(artist . Dixie Chicks)
(rating . 8)
(ripped . #t)
 
(title . Roses)
(artist . Kathy Mattea)
(rating . 7)
(ripped . #t)#t
scheme@(guile-user)> ,pp (select-cd (where-cd 'artist "Dixie Chicks"))
(((title . "Home")
  (artist . "Dixie Chicks")
  (rating . 9)
  (ripped . #t))
 ((title . "Fly")
  (artist . "Dixie Chicks")
  (rating . 8)
  (ripped . #t)))
scheme@(guile-user)> (update-cd (where-cd 'artist "Dixie Chicks")
                                'artist "Abc")
scheme@(guile-user)> (dump-cd-table)
(title . Home)
(artist . Abc)
(rating . 9)
(ripped . #t)
 
(title . Fly)
(artist . Abc)
(rating . 8)
(ripped . #t)
 
(title . Roses)
(artist . Kathy Mattea)
(rating . 7)
(ripped . #t)#t
scheme@(guile-user)> ,pp (select-cd (where-cd 'artist "Abc" 'rating 9))
(((title . "Home")
  (artist . "Abc")
  (rating . 9)
  (ripped . #t)))
scheme@(guile-user)> (save-cd-table "cd.db")
```

上面最后用 `save-cd-table` 过程将 `cd` 数据库保存在 `cd.db` 文件中，当我们需要该数据库时，
只需 `load-cd-table` 过程来加载这个文件即可：

```
scheme@(guile-user)> (create-table cd '(title artist rating ripped))  ; 不可省略该步
scheme@(guile-user)> (load-cd-table "cd.db")
scheme@(guile-user)> (dump-cd-table)
(title . Home)
(artist . Abc)
(rating . 9)
(ripped . #t)
 
(title . Fly)
(artist . Abc)
(rating . 8)
(ripped . #t)
 
(title . Roses)
(artist . Kathy Mattea)
(rating . 7)
(ripped . #t)#t
```
