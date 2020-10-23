#lang racket

;; Binary tree
(struct node-base
  ((value #:mutable)))

(struct node node-base
  (left right))

(define (leaf v)
  (node-base v))

(define (internal v l r)
  (node v l r))

(define (leaf? n)
  (and (node-base? n) (not (node? n))))

(define foo (internal 3 (leaf 2) (leaf 1)))

(define (display-leaf l)
  (if (leaf? l)
      (begin
        (display "[Leaf ")
        (display (node-base-value l))
        (display "]"))
      (display "Not a leaf")))

(define (display-tree t)
  (cond [(leaf? t) (display-leaf t)]
        [(node? t) (begin
                     (display "[Node ")
                     (display (node-base-value t))
                     (display " ")
                     (display-tree (node-left t))
                     (display " ")
                     (display-tree (node-right t))
                     (display "]"))]
        [else (display "Not a tree")]))

(define (tree-map f t)
  (if (leaf? t)
      (leaf (f (node-base-value t)))
      (internal (f (node-base-value t))
                (tree-map f (node-left t))
                (tree-map f (node-right t)))))

(define (tree-map! f t)
  (begin
    (set-node-base-value! t (f (node-base-value t)))
    (when (node? t)
      (begin
        (tree-map! f (node-left t))
        (tree-map! f (node-right t))))
    t))

;; Let's try to define the ++ (increment) operator, like in C
(define (++0 x)
  (begin
    (set! x (+ x 1))
    x))

(define a 1)

(define-syntax ++1
  (syntax-rules ()
    ((_ x)
     (begin
       (set! x (+ x 1))
       x))))

;; Now with multiple variables
(define (+++0 x . remaining)
  (begin
    (++1 x)
    (if (null? remaining)
        (list x)
        (cons x (apply +++0 remaining)))))

(define-syntax +++1
  (syntax-rules ()
    ((_ x) ; (+++1 a)
     (begin
       (++1 x)
       (list x)))
    ((_ x . rest)
     (begin
       (++1 x)
       (if (null? (quote rest))
           (list x)
           (cons x (+++1 . rest)))))))

(define b 2)
(define c 3)
(+++1 a b c)

(define-syntax +++2
  (syntax-rules ()
    ((_) ; (+++2)
     '())
    ((_ x r ...)
     (begin
       (++1 x)
       (cons x (+++2 r ...))))))

(+++2 a b c)

;; Repeat-until like in Pascal
(define-syntax repeat
  (syntax-rules (until)
    ((_ stmt ... until guard)
     (let loop ()
       (begin
         stmt ...)
       (unless guard (loop))))))

(let ((x 1))
  (repeat (++1 x)
          (display x)
          (newline)
   until (> x 10)))


;; Merge sort
(define (merge l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1)) (f2 (car l2)))
                (if (<= f1 f2)
                    (cons f1 (merge (cdr l1) l2))
                    (cons f2 (merge l1 (cdr l2)))))]))

(define (merge-pred pred l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1)) (f2 (car l2)))
                (if (pred f1 f2)
                    (cons f1 (merge-pred pred (cdr l1) l2))
                    (cons f2 (merge-pred pred l1 (cdr l2)))))]))

(define (half-split l)
  (let ((mid (quotient (length l) 2)))
  (cons (take l mid) (drop l mid))))

(define (merge-sort pred l)
  (cond [(null? l) l]
        [(null? (cdr l)) l]
        [else (let ((halves (half-split l)))
                (merge-pred pred
                            (merge-sort pred (car halves))
                            (merge-sort pred (cdr halves))))]))