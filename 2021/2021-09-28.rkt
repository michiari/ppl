#lang racket

(define (merge-pred compare l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (let ((f1 (car l1))
                    (f2 (car l2)))
                (if (compare f1 f2)
                    (cons f1 (merge-pred compare (cdr l1) l2))
                    (cons f2 (merge-pred compare l1 (cdr l2)))))]))

(define (half-split l)
  (let ((half (quotient (length l) 2)))
    (cons (take l half) (drop l half))))

(define (merge-sort compare l)
  (cond [(null? l) '()]
        [(null? (cdr l)) l]
        [else (let ((halves (half-split l)))
                (merge-pred compare
                            (merge-sort compare (car halves))
                            (merge-sort compare (cdr halves))))]))

(struct node-base
  ((value #:mutable)))

(struct node node-base
  (left right))

(define t (node 1 (node 2 (node-base 4) (node-base 5)) (node-base 3)))

(define (leaf? n)
  (and (node-base? n) (not (node? n))))

(define (display-leaf l)
  (if (leaf? l)
      (begin
        (display "[Leaf ")
        (display (node-base-value l))
        (display "]"))
      (display "Not a leaf.")))

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

(define inc (λ (x) (+ x 1)))

(define (tree-map f t)
  (if (leaf? t)
      (node-base (f (node-base-value t)))
      (node (f (node-base-value t))
            (tree-map f (node-left t))
            (tree-map f (node-right t)))))

(define (tree-map! f t)
  (begin
    (set-node-base-value! t (f (node-base-value t)))
    (if (leaf? t)
        t
        (begin
          (tree-map! f (node-left t))
          (tree-map! f (node-right t))
          t))))


;(define (++ x)
;  (set! x (+ x 1))
;  x)

(define-syntax ++
  (syntax-rules ()
    ((_ x)
     (begin
       (set! x (+ x 1))
       x))))

(define a 1)
(++ a)

(define (+++f x . rest)
  (++ x)
  (if (null? rest)
      (list x)
      (cons x (apply +++f rest))))

(define-syntax +++
  (syntax-rules ()
    ((_ x)
     (begin
       (++ x)
       (list x)))
    ((_ x . rest)
     (begin
       (++ x)
       (cons x (+++ . rest))))))

(define b 3)
(define c 4)
(+++ a b c)

(define-syntax +++e
  (syntax-rules ()
    ((_)
     '())
    ((_ x r ...)
     (begin
       (++ x)
       (cons x (+++e r ...))))))

(+++e a b c)
