#lang sicp


(define (factorial n)
(define (iter product counter)
(if (> counter n) product
        (iter (* counter product)
              (+ counter 1))))
(iter 1 1))


(define (count-change amount) (cc amount 5))



(define (cc amount kinds-of-coins)
(cond
  ((= amount 0) 1)
  ((or (< amount 0) (= kinds-of-coins 0)) 0)
  (else (+ (cc amount
               (- kinds-of-coins 1))
           (cc (- amount
                  (first-denomination
                   kinds-of-coins))
               kinds-of-coins)))))


(define (first-denomination kinds-of-coins)
(cond
  ((= kinds-of-coins 1) 1)
  ((= kinds-of-coins 2) 5)
  ((= kinds-of-coins 3) 10)
  ((= kinds-of-coins 4) 25)
  ((= kinds-of-coins 5) 50)))

(define (f n) 
    (cond ((< n 3) n) 
         (else (+ (f (- n 1)) 
                  (* 2 (f (- n 2))) 
                  (* 3 (f (- n 3)))))))

(define (cube x) (* x x x))

(define (sum-integers a b) (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum term a next b) (if (> a b)
                                0
                                (+ (term a)
                                   (sum term (next a) next b))))


(define (inc n) (+ n 1))

(define (sum-cubes a b)
(sum cube a inc b))

(define (pi-sum a b) (define (pi-term x)
(/ 1.0 (* x (+ x 2)))) (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define tst (lambda (x)
              (let ((x 3) (y 5))
                (+ x y))))




(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
(make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
(make-rat (* (numer x) (numer y))
(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
(= (* (numer x) (denom y))
     (* (numer y) (denom x))))


