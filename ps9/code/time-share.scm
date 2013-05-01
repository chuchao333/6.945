;;;; Preemptive scheduling for time-sharing.

(define root-continuation)
(define user-continuation)

(define (setup-time-sharing thunk)
  (set! user-continuation thunk)
  (call-with-current-continuation
   (lambda (k)
     (set! root-continuation k)
     (start-time-sharing)))
  ;; note: when time-sharing terminates the 
  ;;user continuation is run again. 
  (user-continuation))

(define time-sharing:quantum 100) 
(define time-sharing-enabled? #t)
(define time-sharing? #f)

;;; This is an MIT/GNU Scheme specific detail.  register-timer-event
;;; is the MIT/GNU Scheme mechanism for delivering a timer interrupt.
;;; When the time specified by its first argument expires, it invokes
;;; the second argument.

(define (start-time-sharing)
  (set! time-sharing? time-sharing-enabled?)
  (define (setup-interrupt)
    (if time-sharing?
        (register-timer-event time-sharing:quantum on-interrupt)))
  (define (on-interrupt)
    (setup-interrupt)                   ; Recursive interrupt setup
    (call-with-current-continuation
     (lambda (worker-continuation)
       (define (grab-a-continuation)
         (atomically
          (lambda ()
            (queue:add-to-end!
             runnable-actors worker-continuation))))
       (grab-a-continuation)
       (root-continuation 'go))))
  (setup-interrupt))

(define (stop-time-sharing)
  (set! time-sharing? #f))

;;; without-interrupts is an MIT Scheme mechanism that turns off
;;; all timer interrupts.  It executes its thunk atomically.

(define (atomically thunk)
  (without-interrupts thunk))

(define (double-check-lock check do if-not)
  (let ((outside
	 (atomically
	  (lambda ()
	    (if (check)
		(begin (do)
		       (lambda () 'ok))
		if-not)))))
    (outside)))
