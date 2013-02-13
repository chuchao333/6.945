; Problem 1.1.a
;;; define constants
(define symbolic-pi 'symbolic-pi)
(define pi (* 4 (atan 1 1)))

(define-syntax advise-unary
  (syntax-rules ()
    ((advise-unary p wrapper)
     (define p
       (if (and (eq? 1 (procedure-arity-max (procedure-arity p)))
		(eq? 1 (procedure-arity-min (procedure-arity p))))
	   (let ((saved-p p)
		 (ewrapper wrapper))  ;Why rename wrapper? Think macro!
	     (let ((new-p
		    (named-lambda (p x)
		      (ewrapper 'p saved-p x))))
	       (eq-put! new-p 'old-version saved-p)
	       new-p)
	     )
	   (begin (warn "can't advise non-unary procedure") p))
       )
     ))
  )

(define-syntax remove-advice
  (syntax-rules ()
    ((remove-advice p)
     (begin
       (if (eq-get p 'old-version) (set! p (eq-get p 'old-version))
	   (warn "no advice to remove!")
	   )
       p))))

; Problem 1.1.b

(define-syntax advise-binary
  (syntax-rules ()
    ((_ p wrapper)
     (define p
       (if (and (eq? 2 (procedure-arity-max (procedure-arity p)))
		(eq? 2 (procedure-arity-min (procedure-arity p))))
	   (let ((saved-p p)
		 (ewrapper wrapper))  ;Why rename wrapper? Think macro!
	     (let ((new-p
		    (named-lambda (p x1 x2)
		      (ewrapper 'p saved-p x1 x2))))
	       (eq-put! new-p 'old-version saved-p)
	       new-p)
	     )
	   p)
       )
     ))
  )

(define-syntax advise-nary
  (syntax-rules ()
    ((_ p wrapper)
     (define p
       (let ((saved-p p)
	     (ewrapper wrapper))  ;Why rename wrapper? Think macro!
	 (let ((new-p
		(named-lambda (p . args)
		  (ewrapper 'p saved-p args))))
	   (eq-put! new-p 'old-version saved-p)
	   new-p)
	 )
       )
     ))
  )


; problem 1.2
(define (outer-trace-wrapper procname proc args)
  (newline)
  (display ";Entering ") (write procname)
  (display " with ") (write args)
  ;(pp proc)
  (let* (
	 (p (open-output-string))
	 (val (with-output-to-port p (lambda ()
				       (apply proc args)))))
    (newline)
    (display ";Leaving ") (write procname)
    (display " Value=") (write val)
    val))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))

(define (fact n) (fact-iter 1 1 n))

; problem 1.3
(define (authorization-wrapper procname proc args)
  (let* ((key (eq-get proc 'authorization-key)))
    (cond ((or (eqv? #f key)
	       (string=? (md5-string (write-to-string (prompt-for-expression "please authenticate")))
		     key))
	   (apply proc args))
	 (else
	  (error "Unauthorized access" procname)))))

; problem 1.4.a
(define (memo-wrapper-2 procname proc x)
  (let ((seen (assv x (eq-get proc 'old-values))))
    (if seen
	(cdr seen)
	(let ((v (proc x)))
	  (eq-put! proc
		   'old-values
		   (cons (cons x v)
			 (let ((l (eq-get proc 'old-values)))
			   (if
			    (eqv? 2 (length l))
			     (let
				 ((l1 (caar l))
				  (l2 (caadr l)))
			       (if (> l1 l2)
				   (del-assv l2 l)
				   (del-assv l1 l)))
			    l))))
	  v))))

; problem 1.4.b
(define (memo-wrapper-multi procname proc args)
  (let ((seen (assv args (eq-get proc 'old-values))))
    (if seen
	(cdr seen)
	(let ((v (apply proc args)))
	  (eq-put! proc
		   'old-values
		   (cons (cons args v)
			 (eq-get proc 'old-values)))
	  v))))

(define (gcd x y)
  (if (eqv? 0 y)
      x
      (gcd y (remainder x y))))
