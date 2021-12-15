#lang racket

;; A queue
(define queue '())

(define (enqueue x)
  (set! queue (append queue (list x))))

(define (dequeue)
  (unless (null? queue)
    (let ((x (car queue)))
      (set! queue (cdr queue))
      x)))

(define (empty-queue?)
  (null? queue))

;; Coroutines
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
  (displayln (string-append "Exiting " name))
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
