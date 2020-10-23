#lang racket

;; A binary tree
(struct node-base
  ((value #:mutable)))

(struct node node-base
  (left right))

(define (leaf v)
  (node-base v))

(define (internal v l r)
  (node v l r))

(define (leaf? t)
  (and (node-base? t) (not (node? t))))

(define (display-leaf l)
  (if (leaf? l)
      (begin
        (display "[Leaf ")
        (display (node-base-value l))
        (display "]"))
      (display "Not a leaf!")))

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
        [else (display "Not a tree!")]))

(define foo (internal 1 (leaf 2) (leaf 3)))

(define (tree-map f t)
  (if (leaf? t)
      (leaf (f (node-base-value t)))
      (internal (f (node-base-value t))
                (tree-map f (node-left t))
                (tree-map f (node-right t)))))

(define (tree-map! f t)
  (set-node-base-value! t (f (node-base-value t)))
  (unless (leaf? t)
    (begin
      (tree-map! f (node-left t))
      (tree-map! f (node-right t))))
  t)

;; Let's try to define the ++ (increment) operator
(define (++0 x)
  (set! x (+ x 1))
  x)

(define-syntax ++1
  (syntax-rules ()
    ((_ x)
     (begin
       (set! x (+ x 1))
       x))))

(define a 1)

;; Now for multiple variables
(define (+++0 x . rest)
  (begin
    (++1 x)
    (if (null? rest)
        (list x)
        (cons x (apply +++0 rest)))))

(define-syntax +++1
  (syntax-rules ()
    ((_ x)
     (begin
       (++1 x)
       (list x)))
    ((_ x . rest)
     (begin
       (++1 x)
       (if (null? (quote rest))
           (list x)
           (cons x (+++1 . rest)))))))

(define-syntax +++2
  (syntax-rules ()
    ((_)
     '())
    ((_ x r ...)
     (begin
       (++1 x)
       (cons x (+++2 r ...))))))

(define b 2)
(define c 3)
(+++2 a b c)

;; Repeat-until à la Pascal
(define-syntax repeat
  (syntax-rules (until)
    ((_ stmt ... until guard)
     (let loop ()
       (begin
         stmt ...
         (unless guard (loop)))))))

(let ((x 1))
  (repeat
   (++1 x)
   (display x)
   (newline)
   until (> x 10)))

;; To conclude, we finish our merge sort
(define (merge pred l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1))
                    (f2 (car l2)))
                (if (pred f1 f2)
                    (cons f1 (merge pred (cdr l1) l2))
                    (cons f2 (merge pred l1 (cdr l2)))))]))

(define (half-split l)
  (if (null? l)
      l
      (let ((half (quotient (length l) 2)))
        (cons (take l half) (drop l half)))))

(define (merge-sort pred l)
  (cond [(null? l) l]
        [(null? (cdr l)) l]
        [else (let ((halves (half-split l)))
                (merge pred
                       (merge-sort pred (car halves))
                       (merge-sort pred (cdr halves))))]))