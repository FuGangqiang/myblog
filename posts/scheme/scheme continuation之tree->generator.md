created: 2010-11-30T13:51:00+08:00
tags: [scheme]


```
;;;    Function: tree->generator
;;;    Example:
;;;    > (define t '(1 (2) 3))
;;;    > (define gen (tree->generator t))
;;;    > (gen)
;;;    1
;;;    > (gen)
;;;    2
;;;    > (gen)
;;;    3
;;;    > (gen)
;;;    '()
;;;    > (gen)
;;;    '()
 
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