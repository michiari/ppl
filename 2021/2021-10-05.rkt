#lang racket

(define (fibonacci n)
  (cond [(<= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

(define (proj-f n . rest)
  (list-ref rest n))

(define-syntax proj-s
  (syntax-rules ()
    ((_ n v1)
     v1)
    ((_ n v1 vr ...)
     (if (= n 0)
         v1
         (proj-s (- n 1) vr ...)))))

(proj-s 3 (fibonacci 40) 2 3 4 (fibonacci 50))

(define (right-now)
  (call/cc
   (λ (cont)
     (cont "Now"))))

(define (test-cc)
  (display (call/cc
            (λ (escape)
              (begin
                (displayln "cat")
                (escape "paw")
                (displayln "dog"))))))

(define fact-gen #f)
(define (make-fact-gen)
  (let ((f 1) (n 1)) ; f = n!
    (call/cc
     (λ (cont)
       (set! fact-gen cont)))
    (set! f (* f n))
    (set! n (+ n 1))
    f))

(define (some-fact)
  (make-fact-gen)
  (display "moo")
  (fact-gen))

(define (while guard)
  (call/cc
   (λ (back-edge)
     (λ ()
       (when (guard)
         (back-edge (while guard)))))))

(define (count n)
  (let* ((i 0)
         (back-edge (while (λ () (<= i n)))))
    (displayln i)
    (set! i (+ i 1))
    (back-edge)))

(define (break-negative l)
  (call/cc
   (λ (break)
     (for-each (λ (x)
                 (if (< x 0)
                     (break x)
                     (displayln x)))
               l))))

(define (skip-negative l)
  (for-each (λ (x)
              (call/cc
               (λ (continue)
                 (if (< x 0)
                     (continue)
                     (displayln x)))))
            l))

; (while-do guard do (display ...) (set! x ...) break: stop continue: cont)

(define-syntax while-do
  (syntax-rules (do break: continue:)
    ((_ guard do stmt ... break: br-stmt continue: ct-stmt)
     (call/cc
      (λ (br-stmt)
        (let loop ((guard-val (guard)))
          (call/cc
           (λ (ct-stmt)
             (when guard-val
               stmt ...)))
          (when guard-val
            (loop (guard)))))))))

(define (display-positive l)
  (let ((i l))
    (while-do (λ () (pair? l)) do
              (let ((x (car i)))
                (unless (number? x)
                  (stop))
                (set! i (cdr i))
                (unless (> x 0)
                  (cont))
                (displayln x))
              break: stop
              continue: cont)))

; Nondeterministic choices
(define *paths* '())
(define fail #f)
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

(call/cc
 (λ (cc)
   (set! fail
         (λ ()
           (if (null? *paths*)
               (cc 'failure)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))

(define (is-sum-of n)
  (unless (and (>= n 0)
               (<= n 10))
    (error "Not in [0, 10]"))
  (let ((x (choose '(0 1 2 3 4 5)))
        (y (choose '(0 1 2 3 4 5))))
    (if (= (+ x y) n)
        (list x y)
        (fail))))
    
                       
