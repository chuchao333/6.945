(load "load")

(defhandler apply
  (lambda (actor operands calling-environment)
    (if (not (= (length (actor-parameters actor))
		(length operands)))
	(error "Wrong number of operands supplied"))
    (let ((arguments
	   (map (lambda (parameter operand)
		  (evaluate-procedure-operand parameter
					      operand
					      calling-environment))
		(actor-parameters actor)
		operands)))
      (add-to-tasks! actor
		     (lambda ()
		       (eval (actor-body actor)
			     (extend-environment
			      (map procedure-parameter-name
				   (actor-parameters actor))
			      (map (lambda (x) (x)) arguments)
			      (actor-environment actor)))))
      'actor-applied))
  actor-procedure?)

(define evaluate-procedure-operand
  (make-generic-operator 3
			 'evaluate-operand
			 (lambda (parameter operand environment)
			   (lambda () (eval operand environment)))))

;;;(define (lookup-variable-value var env)
;;;  (let plp ((env env))
;;;    (if (eq? env the-empty-environment)
;;;	(lookup-scheme-value var)
;;;	(let scan
;;;	    ((vars (vector-ref env 0))
;;;	     (vals (vector-ref env 1)))
;;;	  (cond ((null? vars) (plp (vector-ref env 2)))
;;;		((eq? var (car vars)) (car vals))
;;;		(else (scan (cdr vars) (cdr vals))))))))
