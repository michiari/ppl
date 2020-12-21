#lang racket

;; First, we make a timer:
(define clock 0)
(define expire-handler #f)

(define (start-timer ticks handler)
  (set! clock ticks)
  (set! expire-handler handler))

(define (decrease-timer)
  (if (> clock 0)
      (set! clock (- clock 1))
      (expire-handler)))

(define (stop-timer)
  (let ((time-left clock))
    (set! clock 0)
    (set! expire-handler #f)
    time-left))

(define (fibonaccit n)
  (decrease-timer)
  (if (< n 2)
      n
      (+ (fibonaccit (- n 1)) (fibonaccit (- n 2)))))

;; (start-timer 30 (λ () (error "Timer expired!")))
;; (fibonacci 3)
;; (fibonacci 10)

;; An engine is created by passing a thunk (procedure of no arguments) to the procedure make-engine.
;; The thunk is the computation to be performed by the engine.
;; An engine is a procedure of three arguments:
;; - ticks, a positive integer that specifies the amount of fuel to be given to the engine.
;;   An engine executes until this fuel runs out or until its computation finishes.
;; - complete, a procedure of two arguments that specifies what to do if the computation finishes.
;;   Its arguments will be the amount of fuel left over and the result of the computation.
;; - expire, a procedure of one argument that specifies what to do if the fuel runs out before the computation finishes.
;;   Its argument is a new engine capable of continuing the computation from the point of interruption.

(define (make-engine proc)
  (let ((do-complete #f) (do-expire #f))
    (define (handler)
      (start-timer (call/cc do-expire) handler))
    
    (define (new-engine start-engine)
      (λ (ticks complete expire)
        (let ((exec-end (call/cc
                         (λ (escape)
                           (set! do-complete (λ (remaining-ticks result)
                                               (escape (λ () (complete remaining-ticks result)))))
                           (set! do-expire (λ (resume) ;; resume is the continuation of the expired task
                                             (escape (λ () (expire (new-engine resume))))))
                           (start-engine ticks)))))
          (exec-end))))

    (new-engine (λ (ticks)
                  (start-timer ticks handler)
                  (let ((result (proc)))
                    (let ((remaining-ticks (stop-timer)))
                      (do-complete remaining-ticks result)))))))

(define fibbo-engine (make-engine (λ () (fibonaccit 6))))
(define (fibbo-resume res-eng)
  (set! fibbo-engine res-eng))
;; (fibbo-engine 10 list fibbo-resume)

;; Some syntactic sugar for the timer
(define-syntax timed-lambda
  (syntax-rules ()
    ((_ formal-args exp1 exp2 ...)
     (lambda formal-args
       (decrease-timer)
       exp1 exp2 ...))))

;; Now we can use timed-lambda to easily create functions that make the timer tick.
(define fibonacci
  (timed-lambda (n)
                (if (< n 2)
                    n
                    (+ (fibonaccit (- n 1)) (fibonaccit (- n 2))))))

;; Try using timers to make a round-robin scheduler.
;; The scheduler takes a list of engines and a "quantum" value, which is the amount of time each engine
;; can take before it is suspended.
;; The scheduler runs each engine for at most "quantum" time ticks.
;; When the quantum expires, the next engine is run, and the one that expired is scheduled to be run after all the others.
;; When all tasks are completed, the scheduler resurns a list with their results.
;; Try to do this by yourself, before looking at the solution!

;; E.g.,
(round-robin (map (λ (x)
                    (make-engine (λ () (fibonacci x))))
                  (range 10))
             5)
;; returns '(0 1 1 2 3 5 8 13 21 34)





;; Solution:
(define (round-robin engines quantum)
  (if (null? engines)
      '()
      ((car engines)
       quantum
       (λ (ticks value)
         (cons value (round-robin (cdr engines) quantum)))
       (λ (res-eng)
         (round-robin (append (cdr engines) (list res-eng)) quantum)))))



