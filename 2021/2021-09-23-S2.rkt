#lang racket

(define (hello-world)
  (display "Hello world"))

(define (hello name)
  (display (string-append "Hello " (~a name))))

(define n 42)
(define hello2 (λ (name)
                 (display (string-append "Hello " (~a name)))))

;; n + (n-1) + ... + 2 + 1 + 0
;; f(0) = 0
;; f(n) = n + f(n-1)

(define (sum-range n)
  (if (<= n 0)
      0
      (+ n (sum-range (- n 1)))))

;; n OP ((n-1) OP (... OP ((e+2) OP ((e+1) OP e))))
;; f(0) = 0
;; f(n) = n OP f(n-1)

(define (fold-range op n e)
  (if (<= n e)
      e
      (op n (fold-range op (- n 1) e))))

;; (((((n OP (n-1)) OP ...) OP (e+2)) OP (e+1)) OP e

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
  (if (not (pair? p))
      (error (string-append (~a p) " is not a pair."))
      (let ((fst (car p))
            (snd (cdr p)))
        (cons snd fst))))

(define (tsil l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list."))]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))

(define (fold-range2 op n start e)
  (if (<= n start)
      e
      (op n (fold-range2 op (- n 1) start e))))

;; '(((1) 2) 3 (((4)) 3) 4 (((3))))
;; '(1 2 3 4 3 4 3)

(define (flatten l)
  (cond [(null? l) l]
        [(not (list? l)) (list l)]
        [else (append (flatten (car l)) (flatten (cdr l)))]))

;; '(3 5 1 3 73 44 4 3)
;; '(3 5 1 3) '(73 44 4 3)
;; ...
;; '(1 3 3 5) '(3 4 44 73)
;; '() '(44 73)
;; '(1 3 3 3 4 5 44 73)

(define (merge l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1))
                    (f2 (car l2)))
                (if (<= f1 f2)
                    (cons f1 (merge (cdr l1) l2))
                    (cons f2 (merge l1 (cdr l2)))))]))

;; Try to code merge sort.
