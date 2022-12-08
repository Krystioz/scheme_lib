#lang sicp





(define x (list 1 2 3))
(define y (list 4 5 6))


(append x y)
(cons x y)
(list x y)

(define (scale-tree tree factor) (cond ((null? tree) nil)
((not (pair? tree)) (* tree factor))
(else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))