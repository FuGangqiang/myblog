created: 2010-11-28T13:48:00+08:00
tags: [scheme]


```
;;;  amb
;;;  Example:
;;;    > (if (amb #f #t)
;;;          1
;;;          (amb))
;;;    1
;;;    > (let ((x (list (amb 1 4 5)
;;;                     (amb 2 6 7))))
;;;        (let ((x1 (car x))
;;;              (x2 (cadr x)))
;;;          (amb:assert (and (even? x1)
;;;                           (even? x2)))
;;;          (amb:assert (< x1 x2)))
;;;        x)
;;;    (4 6)
 
(define amb:fail #f)
 
(define (amb:init-fail)
  (set! amb:fail
        (lambda () (error "amb tree exhausted"))))
(amb:init-fail)
 
(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((amb:prev-fail amb:fail))
       (call/cc
        (lambda (choose)
           
          (call/cc
           (lambda (jump)
             (set! amb:fail
                   (lambda()
                     (set! amb:fail amb:prev-fail)
                     (jump 'next)))            
             (choose alt))) ...
          
         (amb:prev-fail)))))))
 
(define (amb:assert pred)
  (if (not pred)
      (amb)))
```