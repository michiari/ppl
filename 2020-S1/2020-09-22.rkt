#lang racket

;; Let us define a variable...
(define hw "hello world")

;; ... and a function:
(define (hello x)
  (display (string-append "hello " x)))

;; n >= 0
;; n + (n-1) + ... + 1 + 0
;; f(0) = 0
;; f(n) = n + f(n-1)

(define (sum-range n)
  (if (<= n 0)
      0
      (+ n (sum-range (- n 1)))))

;; n >= 0
;; OP is an associative binary operator
;; n OP (n-1) OP ... (e+2) OP (e+1) OP e
;; f(0) = 0
;; f(n) = n OP f(n-1)

(define (fold-range op n e)
  (if (<= n e)
      e
      (op n (fold-range op (- n 1) e))))


;; e OP n OP (n-1) OP ... (e+2) OP (e+1)

(define (fold-range-tail op n e)
  (define (fold-range-tail-aux op n acc)
    (if (<= n e)
        acc
        (fold-range-tail-aux op (- n 1) (op n acc))))
  (fold-range-tail-aux op n e))


;; n OP (n-1) OP ... (e+2) OP (e+1) OP e

(define (fold-range-tail2 op n e)
  (define (fold-range-tail-aux op n acc)
    (if (<= n e)
        acc
        (fold-range-tail-aux op (- n 1) (op n acc))))
  (fold-range-tail-aux op (- n 1) n))

;; now with a named let:
(define (fold-range-it op n e)
  (let count ((x (- n 1)) (acc n))
    (if (<= x e)
        acc
        (count (- x 1) (op x acc)))))

;; Variables can be bound to lists, of course:
(define mylist (cons 2 '(3 4)))

;; Reverse pair:
(define (riap p)
  (if (not (pair? p))
      (error (string-append (~a p) " is not a pair!"))
      (let ((f (car p))
            (s (cdr p)))
        (cons s f))))

;; More concise:
(define (riap2 p)
  (if (not (pair? p))
      (error (string-append (~a p) " is not a pair!"))
      (cons (cdr p) (car p))))

;; Reverse list:
(define (tsil l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list."))]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))

;; Flatten list (e.g. (flatten '(((1) 2) 3 ((4)))) --> '(1 2 3 4)):
(define (flatten l)
  (cond [(null? l) l]
        [(not (list? l)) (list l)]
        [else (append (flatten (car l)) (flatten (cdr l)))]))

