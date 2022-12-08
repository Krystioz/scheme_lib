#lang racket


(define insertL (lambda (new old l)
                  (cond ((null? l) (quote ()))
                        (else (cond ((eq? (car l) old) (cons new (cons old (cdr l))))
                                    (else (cons (car l) (insertL new old (cdr l)))))))))