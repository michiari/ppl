#lang racket

;; A projection function
(define (proj-f n . rest)
  (list-ref rest n))

(define (fibonacci n)
  (cond [(<= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

;; What happens with
;; (proj 3 1 (+ 2 3) 3 4 (fibonacci 42) 5)?

;; A better way to define it
(define-syntax proj
  (syntax-rules ()
    ((_ n v)
     v)
    ((_ n v1 vr ...)
     (if (= n 0)
         v1
         (proj (- n 1) vr ...)))))

;; (proj 3 1 (+ 2 3) 3 4 (fibonacci 42) 5)


;; Continuations

;; A useless one
(define (right-now)
  (call/cc
   (λ (cont)
     (cont "Now"))))

(define (test-cc)
  (displayln
   (call/cc
    (λ (escape)
      (displayln "Foo")
      (escape "Bar")
      (displayln "Baz")))))

;; Factorial generator
(define fact-gen #f)
(define (set-fact-gen)
  (let ((f 1) (n 1))
    (call/cc
     (λ (cont)
       (set! fact-gen cont)))
    (set! f (* f n))
    (set! n (+ n 1))
    f))

;; Do-while loop
(define (make-while cond)
  (call/cc
   (λ (back-edge)
     (λ ()
       (when (cond)
         (back-edge (make-while cond)))))))

(define (count n)
  (let* ((i 0)
        (while (make-while (λ () (< i n)))))
    (displayln i)
    (set! i (+ i 1))
    (while)))

;; Break statement
(define (break-negative l)
  (call/cc
   (λ (break)
     (for-each (λ (x)
                 (if (>= x 0)
                     (displayln x)
                     (break)))
               l))))

;; Continue statement
(define (skip-negative l)
  (for-each (λ (x)
              (call/cc
               (λ (continue)
                 (if (>= x 0)
                     (displayln x)
                     (continue)))))
            l))

;; While with break and continue
(define-syntax while-do
  (syntax-rules (do break: continue:)
    ((_ cond do stmt ... break: br-stmt continue: ct-stmt)
     (call/cc
      (λ (br-stmt)
        (let loop ((guard-val (cond)))
          (call/cc
           (λ (ct-stmt)
             (when guard-val
               stmt ...)))
          (when guard-val
            (loop (cond)))))))))

(define (display-positive l)
  (let ((i l))
    (while-do (λ ()
                (pair? i))
              do
              (let ((x (car i)))
                (unless (number? x)
                  (break))
                (set! i (cdr i))
                (unless (> x 0)
                  (continue))
                (displayln x))
              break: break
              continue: continue)))

;; Nondeterministic choices with backtracking
(define fail #f)
(define *paths* '())
(define (choose l)
  (if (null? l)
      (fail)
      (call/cc
       (λ (cc)
         (set! *paths*
               (cons (λ ()
                       (cc (choose (cdr l))))
                     *paths*))
         (car l)))))

(call/cc
 (λ (exit)
   (set! fail (λ ()
                (if (null? *paths*)
                    (exit 'failure)
                    (let ((p1 (car *paths*)))
                      (set! *paths* (cdr *paths*))
                      (p1)))))))

(define (is-the-sum-of sum)
  (unless (and (>= sum 0) (<= sum 10))
    (error sum "Out of range"))
  (let ((x (choose '(0 1 2 3 4 5)))
        (y (choose '(0 1 2 3 4 5))))
    (if (= (+ x y) sum)
        (list x y)
        (fail))))

;; Coroutines
(define *queue* '())

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (empty-queue?)
  (null? *queue*))

(define (start-coroutine proc)
  (call/cc
   (λ (cc)
     (enqueue cc)
     (proc))))

(define (yield)
  (call/cc
   (λ (cc)
     (enqueue cc)
     ((dequeue)))))

(define (exit-coroutine name)
  (displayln (string-append "Terminating coroutine " name))
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define (say-something name n)
  (λ ()
    (let loop ((i 0))
      (display name)
      (display ": ")
      (displayln i)
      (yield)
      (if (< i n)
          (loop (+ i 1))
          (exit-coroutine name)))))

(define (test-cr)
  (start-coroutine (say-something "A" 3))
  (start-coroutine (say-something "B" 2))
  (exit-coroutine "Main"))
