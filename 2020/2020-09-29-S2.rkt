#lang racket

(define hw "Hello world!")

(define (helloworld)
  (display hw))

(define (hello person)
  (display (string-append "Hello " (~a person))))

(define hello2 (lambda (person)
                 (display (string-append "Hello " person))))

;; n + (n-1) + ... + 1 + 0
;; f(0) = 0
;; f(n) = n + f(n-1)

(define (sum-range n)
  (if (= n 0)
      0
      (+ n (sum-range (- n 1)))))

;; n op (n-1) op ... (e+2) op (e+1) op e

(define (fold-range op n e)
  (if (<= n e)
      e
      (op n (fold-range op (- n 1) e))))

;; A tail-recursive version
(define (fold-range-tail op n e)
  (define (fold-range-tail-aux n acc)
    (if (<= n e)
        acc
        (fold-range-tail-aux (- n 1) (op n acc))))
  (fold-range-tail-aux (- n 1) n))

;; An "iterative" version
(define (fold-range-it op n e)
  (let count ((x (- n 1)) (acc n))
    (if (<= x e)
        acc
        (count (- x 1) (op x acc)))))

;; Reverse pair
(define (riap p)
  (if (not (pair? p))
      (error (string-append (~a p) " is not a pair!"))
      (cons (cdr p) (car p))))

;; Reverse list
(define (tsil l)
  (if (not (list? l))
      (error (string-append (~a l) " is not a list!"))
      (if (null? l)
          l
          (append (tsil (cdr l)) (list (car l))))))

(define (tsil2 l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list!"))]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))

;; Flatten list
(define (flatten l)
  (cond [(not (list? l)) (list l)]
        [(null? l) l]
        [else (append (flatten (car l)) (flatten (cdr l)))]))

;; Merge sorted lists
(define (merge l1 l2)
  (cond [(not (and (list? l1) (list? l2))) (error "A non-list argument was supplied.")]
        [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((h1 (car l1))
                    (h2 (car l2)))
                (if (<= h1 h2)
                    (cons h1 (merge (cdr l1) l2))
                    (cons h2 (merge l1 (cdr l2)))))]))

;; Generic version
(define (merge-pred pred l1 l2)
  (cond [(not (and (list? l1) (list? l2))) (error "A non-list argument was supplied.")]
        [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((h1 (car l1))
                    (h2 (car l2)))
                (if (pred h1 h2)
                    (cons h1 (merge-pred pred (cdr l1) l2))
                    (cons h2 (merge-pred pred l1 (cdr l2)))))]))