#lang racket

(define (hello-world)
  (display "hello world"))

(define (hello name)
  (display (string-append "Hello " (~a name))))

(define hello2 (λ (name)
                 (display (string-append "Hello " (~a name)))))

;; n + (n-1) + ... + (2 + (1 + 0))
;; f(0) = 0
;; f(n) = n + f(n-1)
;; f(3) = 3 + f(2) = 3 + 2 + f(1) = 3 + 2 + 1 + f(0) = 3 + 2 + 1 + 0

(define (sum-range n)
  (if (<= n 0)
      0
      (+ n (sum-range (- n 1)))))

;; n OP (n-1) OP ... OP ((e-2) OP ((e-1) OP e))
;; f(0) = 0
;; f(n) = n OP f(n-1)

(define (fold-range op n e)
  (if (<= n e)
      e
      (op n (fold-range op (- n 1) e))))

;; ((((n OP (n-1)) OP ...) OP (e-2)) OP (e-1)) OP e

(define (fold-range-tail op n e)
  (define (fold-range-tail-aux x acc)
    (if (<= x e)
        (op acc e)
        (fold-range-tail-aux (- x 1) (op acc x))))
  (fold-range-tail-aux (- n 1) n))

(define (fold-range-it op n e)
  (let count ((x (- n 1)) (acc n))
    (if (<= x e)
        (op acc e)
        (count (- x 1) (op acc x)))))

(define (riap p)
  (if (pair? p)
      (let ((fst (car p)) (snd (cdr p)))
        (cons snd fst))
      (error (string-append (~a p) " is not a pair."))))

(define (fold-range-first op n first e)
  (if (<= n first)
      e
      (op n (fold-range-first op (- n 1) first e))))

(define (fold-range-tail-first op n first e)
  (define (fold-range-tail-aux x acc)
    (if (<= x first)
        (op acc e)
        (fold-range-tail-aux (- x 1) (op acc x))))
  (fold-range-tail-aux (- n 1) n))

(define (tsil l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list!"))]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))

;; '((1 4) 3 (1 2 (4)) (((4))))
;; '(1 4 3 1 2 4 4)

(define (flatten l)
  (cond [(null? l) l]
        [(not (list? l)) (list l)]
        [else (append (flatten (car l)) (flatten (cdr l)))]))

;; '(2 1 5 1 3)
;; '(2 1 5) '(1 3)
;; '(1 2 5) '(1 3)
;; '(1 1 2 3 5)

(define (merge l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1)) (f2 (car l2)))
                (if (<= f1 f2)
                    (cons f1 (merge (cdr l1) l2))
                    (cons f2 (merge l1 (cdr l2)))))]))

;; Use merge to implement merge sort.
