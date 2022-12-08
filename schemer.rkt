#lang racket

(define member? (lambda (wrd arr)(
                                  cond ((null? arr) #f)
                                       (else (or (eq? (car arr) wrd) (member? wrd (cdr arr)))))))



(define subst2 (lambda (new old old2 lat)
                (cond ((null? lat) (quote ()))
                      ((or (eq? (car lat) old) (eq? (car lat) old2)) (cons new (cdr lat)))
                      (else (cons (car lat) (subst2 new old old2 (cdr lat)))))))