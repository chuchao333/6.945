;;; load definitions
(load "load.scm")
(load "ps1_defs.scm")

; Problem 1.1.a
;;; advise sin
(advise-unary sin
	      (lambda (name oldf x)
		(cond ((eq? x symbolic-pi) 0)
		      ((number? x) (oldf x))
		      (else
		       (error "Argument not number" name)))))
;;; show advice works
(sin symbolic-pi)
(sin pi)
;;; remove advice
(remove-advice sin)
;;; removing advice from un-advised function generates a warning,
;;; but leaves the function unchanged
(remove-advice cos)
;;; show that cos is still a valid procedure
(procedure? cos)
;;; show that calling advise-unary on a non-unary function does nothing
(advise-unary +
	      (lambda (name oldf x)
		(error "uhoh, shouldn't advise non-unary functions!" name)))
(+ 2 2)
(advise-unary cons
	      (lambda (name oldf x)
		(error "uhoh, shouldn't advise non-unary functions!" name)))
(cons 'a '(b))

; Problem 1.1.b
;;; but advise-binary and advise-unary work!
(advise-binary cons
	      (lambda (name oldf x1 x2)
		(begin (display "aha! it works")
		       (oldf x1 x2))))
(cons 'a '(b))
(remove-advice cons)
;;; remove advice from cons, since other methods call it ;)
(advise-nary +
	     (lambda (name oldf args)
	       (begin (display "aha! it works") (write name)
		      (display " with ") (write args)
		      (apply oldf args))))
(+ 2 3 4)
(remove-advice +)




; problem 1.2
(remove-advice fact-iter)
(advise-nary fact-iter full-trace-wrapper)
(fact 4)
(display "testing")
(remove-advice fact-iter)

(pp fact-iter)
(advise-nary fact-iter outer-trace-wrapper)
(pp fact-iter)
(fact 4)
(remove-advice fact-iter)

;(advise-nary sin outer-trace-wrapper)
;(sin pi)
;(remove-advice sin)

; problem 1.3

(eq-put! sin 'authorization-key (md5-string "test"))
(advise-nary sin authorization-wrapper)
(write (sin 1))
(remove-advice sin)

; further, if we don't set password, anyone can use it!
(advise-nary cos authorization-wrapper)
(write (cos 1))
(remove-advice cos)

; problem 1.4

(eq-put! fib 'old-values '())
(advise-unary fib memo-wrapper-2)

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

(remove-advice fib)

(show-time
 (lambda ()
   (gcd 99999999 12)))

(eq-put! gcd 'old-values '())
(advise-nary gcd memo-wrapper-multi)
(show-time
 (lambda ()
   (gcd 99999999 3)))
(remove-advice gcd)
