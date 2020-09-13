#lang racket

;; Let us see some continuations
(define (right-now)
  (call/cc
   (λ (cc)
     (cc "Now"))))

(define (test-cc)
  (display (call/cc
            (λ (escape)
              (display "cat\n")
              (escape "paw\n")
              "bacon\n"))))

;; We can make a generator of factorials
(define fact-gen #f)
(define (get-fact-gen)
  (let ((f 1) (n 1))
    (call/cc
     (λ (cont)
       (set! fact-gen cont)))
    (set! f (* f n))
    (set! n (+ n 1))
    f))

;; Try (get-fact-gen)
;;     (fact-gen)
;;     (define fact-gen2 fact-gen)
;;     (fact-gen)
;;     (fact-gen2)

;; This never terminates. Why?
(define (some-fact)
  (get-fact-gen)
  (displayln "asd")
  (fact-gen))

;; We can make a break statement
(define (break-negative)
  (call/cc
   (λ (break)
     (for-each (λ (x)
                 (if (>= x 0)
                     (begin
                       (display x)
                       (newline))
                     (break)))
               '(0 1 2 -3 4 5)))))

;; Or a continue statement
(define (skip-negative)
  (for-each (λ (x)
              (call/cc
               (λ (continue)
                 (if (>= x 0)
                     (displayln x)
                     (continue 'neg)))))
            '(0 1 2 -3 4 5)))

;; And a while with break and continue
(define-syntax while-do
  (syntax-rules (od break: continue:)
    ((_ guard od stmt ... break: br-stmt continue: cont-stmt)
     (call/cc
      (λ (br-stmt)
        (let loop ((guard-val (guard)))
          (call/cc
           (λ (cont-stmt)
             (when guard-val
               stmt ...)))
           (when guard-val
             (loop (guard)))))))))

(define (display-positive l)
  (let ((i l))
    (while-do (λ ()
                (pair? i))
              od
              (let ((x (car i)))
                (unless (number? x)
                  (stop))
                (set! i (cdr i))
                (unless (>= x 0)
                  (cont))
                (displayln x))
              break: stop
              continue: cont)))
;; Try (display-positive '(0 1 2 3 -5 4 'asd 9))

;; Nondeterministic choices

; First, we need a FIFO queue
(define *paths* '())
(define (choose choices)
  (if (null? choices)
      (fail)
      (call/cc
       (λ (cc)
         (set! *paths*
               (cons (λ ()
                       (cc (choose (cdr choices))))
                     *paths*))
         (car choices)))))

(define fail #f)
(call/cc
 (λ (cc)
   (set! fail
         (λ ()
           (if (null? *paths*)
               (cc 'failure)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))

              
(define (is-the-sum-of sum)
  (unless (and (>= sum 0)
               (<= sum 10))
    (error "out of range"))
  (let ((x (choose '(0 1 2 3 4 5)))
        (y (choose '(0 1 2 3 4 5))))
    (if (= (+ x y) sum)
        (list x y)
        (fail))))


(define *queue* '())
(define (enqueue x)
  (set! *queue* (append *queue* (list x))))
(define (dequeue)
  (unless (empty-queue?)
    (let ((x (car *queue*)))
      (set! *queue* (cdr *queue*))
      x)))
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
  (displayln (string-append name ": exit."))
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
  (exit-coroutine "main"))

;; Try (test-cr)
