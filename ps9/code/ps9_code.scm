;(load "load")
;;; Problem 8.2b
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
