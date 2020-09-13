#lang racket

;; Function that splits a list in two halves
(define (half-split-simple l)
  (let ((half (ceiling (/ (length l) 2))))
    (cons (take l half) (drop l half))))

;; Same, but hand-crafted.
(define (half-split l)
  (let split ((sec l)
              (last l))
    (cond [(null? last) (cons '() sec)]
          [(null? (cdr last)) (cons (list (car sec)) (cdr sec))]
          [else (let ((halves (split (cdr sec) (cddr last))))
                  (cons (cons (car sec) (car halves)) (cdr halves)))])))

;; Structs

;; Binary tree

(struct node-base
  ((value #:mutable)))

(struct node node-base
  (left right))

(define (create-leaf v)
  (node-base v))

(define (create-internal v l r)
  (node v l r))

(define (leaf? n)
  (and (node-base? n) (not (node? n))))

(define (display-leaf l)
  (if (leaf? l)
      (begin
        (display "[Leaf ")
        (display (node-base-value l))
        (display "]"))
      (display "NOt a leaf!")))

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
        [else (display "NOt a tree.")]))

(define foo (node 3 (node-base 2) (node-base 1)))

(define inc (lambda (x) (+ x 1)))

(define (tree-map f t)
  (if (leaf? t)
      (node-base (f (node-base-value t)))
      (node (f (node-base-value t))
            (tree-map f (node-left t))
            (tree-map f (node-right t)))))

(define (tree-map! f t)
  (begin
    (if (leaf? t)
        (set-node-base-value! t (f (node-base-value t)))
        (begin
          (set-node-base-value! t (f (node-base-value t)))
          (tree-map! f (node-left t))
          (tree-map! f (node-right t))))))

;; Macros

(define-syntax ++
  (syntax-rules ()
    ((_ i)
     (begin
       (set! i (+ 1 i))
       i))))

(define a1 1)
(++ a1)

(define (+++ x . rest)
  (begin
    (++ x)
    (if (null? rest)
        (list x)
        (cons x (apply +++ rest)))))

(define a2 2)
(define a3 3)
(+++ a1 a2 a3) ;; +++ this does not modify a1 a2 and a3! We need a macro.

;; This way:
(define-syntax ++++
  (syntax-rules ()
    ((_ x)
     (begin
       (++ x)
       (list x)))
    ((_ x . rest)
     (begin
       (++ x)
       (cons x (++++ . rest))))))

(++++ a1 a2 a3)

;; Or this way:
(define-syntax ++++p
  (syntax-rules ()
    ((_)
     '())
    ((_ x r ...)
     (begin
       (++ x)
       (cons x (++++p r ...))))))

(++++p a1 a2 a3)

;; Pascal's repeat-until.
(define-syntax repeat
  (syntax-rules (until)
    ((_ stmt ... until cond)
     (let loop ()
       (begin
         stmt ...
         (unless cond
           (loop)))))))

(let ((x 1) (y 10))
  (repeat (++ x)
          (display x)
          (newline)
          until (> x y)))

