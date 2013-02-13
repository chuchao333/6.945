;;;; advice.scm

;;; This file contains code fragments that appear in the text of the
;;; problem set, relieving you of the need to type them in.


(define-syntax advise-unary
  (syntax-rules ()
    ((advise-unary p wrapper)
     (define p
       (let ((saved-p p)
	     (ewrapper wrapper))  ;Why rename wrapper? Think macro!
	 (let ((new-p 
		(named-lambda (p x)
		  (ewrapper 'p saved-p x))))
	   (eq-put! new-p 'old-version saved-p)
	   new-p))))))

(define-syntax remove-advice
  (syntax-rules ()
    ((remove-advice p)
     (begin (set! p (eq-get p 'old-version))
	    'done))))


(define (full-trace-wrapper procname proc args)
  (newline)
  (display ";Entering ") (write procname)
  (display " with ") (write args)
  (let ((val (apply proc args)))
    (newline)
    (display ";Leaving ") (write procname)
    (display " Value=") (write val)
    val))

#|
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))

(define (fact n) (fact-iter 1 1 n))
|#


(define (authorization-wrapper procname proc args)
   (cond ((memq (eq-get proc 'authorization-key)
		(or (eq-get user-id 'authorizations)
		    '()))
	  (apply proc args))
	 (else
	  (error "Unauthorized access" user-id procname))))


#|
     (advise-nary sin authorization-wrapper)
     (eq-put! sin 'authorization-key 'ok-to-sin)

     (eq-put! gjs 'authorizations
              `(ok-to-sin ok-to-cos ok-to-atan ok-to-read-files))
|#


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 2))
	 (fib (- n 1)))))

#|
(show-time
 (lambda ()
   (fib 30)))
;process time: 1510 (1500 RUN + 10 GC); real time: 1512
;Value: 832040

(show-time
 (lambda ()
   (fib 31)))
;process time: 2430 (2410 RUN + 20 GC); real time: 2423
;Value: 1346269
|#

(define (memo-wrapper procname proc x)
  (let ((seen (assv x (eq-get proc 'old-values))))
    (if seen
	(cdr seen)
	(let ((v (proc x)))
	  (eq-put! proc 
		   'old-values
		   (cons (cons x v)
			 (eq-get proc 'old-values)))
	  v))))

(eq-put! fib 'old-values '())
(advise-unary fib memo-wrapper)

#|
(show-time
 (lambda ()
   (fib 30)))
;process time: 0 (0 RUN + 0 GC); real time: 0
;Value: 832040

(show-time
 (lambda ()
   (fib 100)))
;process time: 0 (0 RUN + 0 GC); real time: 0
;Value: 354224848179261915075
|#