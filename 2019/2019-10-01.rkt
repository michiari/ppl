#lang racket

;; Let us define a variable.
(define hw "Hello world!")

;; Let us define a variable.
(define (hello)
  (display hw))

;; Try (display (hello))
;; Try (display hello)

;; We can do it as a lambda as well.
(define world
  (lambda () (display hw)))

;; Given an associative binary operator OP and a positive Natural number n, we want to compute 1 OP 2 OP 3 OP ... OP n
;; Let us try to do it with `+'.
(define (sum-range n)
  (if (<= n 1)
      1
      (+ n (sum-range (- n 1)))))

;; Now we generalize it
(define (fold-range op n e)
  (if (<= n 0)
      e
      (op n (fold-range op (- n 1) e))))

;; Try this:
(fold-range + 42 0)
(fold-range * 6 1)

;; Tail-recursive
(define (fold-range-tail op n e)
  (define (fold-range-tail-aux n acc)
    (if (<= n 0)
        acc
        (fold-range-tail-aux (- n 1) (op n acc))))
  (fold-range-tail-aux n e))

;; Try this:
(fold-range-tail + 42 0)
(fold-range-tail * 6 1)

;; "Iterative"
(define (fold-range-it op n e)
  (let count ((x n) (acc e))
    (if (<= x 0)
        acc
        (count (- x 1) (op x acc)))))

;; Try this:
(fold-range-it + 42 0)
(fold-range-it * 6 1)

;; Now try this:
(fold-range-it (lambda (x y)
                  (+ (* x x) y))
                10
                0)
(fold-range-tail (lambda (x y)
                    (+ (* x x) y))
                  10
                  0)
(fold-range (lambda (x y)
               (+ (* x x) y))
             10
             0)

;; Let us do something with lists
(define thelist '(1 2 3))
(cons 1 2)
(list? (cons 1 2))
(pair? thelist)
(cons 1 (cons 2 (cons 3 '())))
(car thelist)
(car (cdr (cdr thelist)))
(cadr thelist) ; aka (car (cdr l))
(caddr thelist) ; aka (car (cdr (cdr l)))

;; And now this:
(fold-range-it cons 42 '())
(fold-range-tail cons 42 '())
(fold-range cons 42 '())

;; Reverse pair
(define (riap p)
  (if (not (pair? p))
      (error (append (~a p) " is not a pair!"))
      (let ((f (car p))
            (s (cdr p)))
        (cons s f))))

;; Reverse list
(define (tsil l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list!"))]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))

;; Flatten list
(define (flatten l)
  (cond [(null? l) l]
        [(not (list? l)) (list l)]
        [else (append (flatten (car l)) (flatten (cdr l)))]))

;; Try this:
(flatten '(1 2 3 4))
(flatten '((1) 2 ((3 4) 5) (((6)))))

;; Sorted list merge
(define (merge l1 l2)
  (cond [(not (and (list? l1) ( list? l2))) (error "Supplied with non-list argument!")]
        [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1)) (f2 (car l2)))
                (if (<= f1 f2)
                    (cons f1 (merge (cdr l1) l2))
                    (cons f2 (merge l1 (cdr l2)))))]))

;; More general
(define (merge-gen compar l1 l2)
  (cond [(not (and (list? l1) ( list? l2))) (error "Supplied with non-list argument!")]
        [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1)) (f2 (car l2)))
                (if (compar f1 f2)
                    (cons f1 (merge-gen compar (cdr l1) l2))
                    (cons f2 (merge-gen compar l1 (cdr l2)))))]))

(define (half-split l)
 (cons '("Homework") '("Homework")))

;; Merge-sort algorithm
(define (merge-sort compar l)
  (cond [(not (list? l)) (error (string-append (~a l) " is not a list."))]
        [(null? l) l]
        [(null? (cdr l)) l]
        [else (let ((halves (half-split l)))
                (merge-gen compar
                           (merge-sort compar (car halves))
                           (merge-sort compar (cdr halves))))]))
