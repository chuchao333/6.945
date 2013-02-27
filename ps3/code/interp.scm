;;; Core of extended Scheme interpreter
;;;
;;; See also structure and predicate definitions in rtdata.scm and
;;; syntax.scm

(declare (usual-integrations eval apply))

(define (default-eval expression environment)
  (cond ((application? expression)
	 (apply (eval (operator expression) environment)
		(operands expression)
		environment))
	(else
	 (error "Unknown expression type" expression))))

(define (default-apply procedure operands calling-environment)
  (error "Unknown procedure type" procedure))



(define eval
  (make-generic-operator 2 'eval default-eval))

(defhandler eval
  (lambda (expression environment) expression)
  self-evaluating?)

(defhandler eval lookup-variable-value variable?)

(defhandler eval
  (lambda (expression environment)
    (text-of-quotation expression))
  quoted?)

(defhandler eval
  (lambda (expression environment)
    (make-compound-procedure
     (lambda-parameters expression)
     (lambda-body expression)
     environment))
  lambda?)

(defhandler eval
  (lambda (expression environment)
    (if (eval (if-predicate expression) environment)
	(eval (if-consequent expression) environment)
	(eval (if-alternative expression) environment)))
  if?)

(defhandler eval
  (lambda (expression environment)
    (eval (cond->if expression) environment))
  cond?)

(defhandler eval
  (lambda (expression environment)
    (eval (let->combination expression) environment))
  let?)

(defhandler eval
  (lambda (expression environment)
    (evaluate-sequence (begin-actions expression)
		       environment))
  begin?)

(define (evaluate-sequence actions environment)
  (cond ((null? actions)
	 (error "Empty sequence"))
	((null? (rest-exps actions))
	 (eval (first-exp actions) environment))
	(else
	 (eval (first-exp actions) environment)
	 (evaluate-sequence (rest-exps actions) environment))))

(defhandler eval
  (lambda (expression environment)
    (define-variable! (definition-variable expression)
      (eval (definition-value expression) environment)
      environment)
    (definition-variable expression))
  definition?)

(defhandler eval
  (lambda (expression environment)
    (set-variable-value! (assignment-variable expression)
      (eval (assignment-value expression) environment)
      environment))
  assignment?)

(define apply
  (make-generic-operator 3 'apply default-apply))


(define (symbolic? expression)
  (if (pair? expression)
      (or (symbolic? (car expression)) (symbolic? (cdr expression)))
      (and (not (null? expression)) (symbol? expression))))

(define (procedure-to-symbol proc env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(procedure-to-symbol-scheme proc generic-evaluation-environment)
	(let scan
	    ((vars (environment-variables env))
	     (vals (environment-values env)))
	  (cond ((null? vars) (plp (environment-parent env)))
		((eq? proc (car vals)) (car vars))
		(else (scan (cdr vars) (cdr vals))))))))

(define (procedure-to-symbol-scheme proc env)
  (let plp ((env env))
    (let scan
	((bindings (environment-bindings env)))
      (cond ((null? bindings)
	     (if (environment-has-parent? env)
		 (plp (scheme-environment-parent env))
		 proc))
	    ((and (= (length (car bindings)) 2) (eq? proc (cadar bindings)))
	     (caar bindings))
	    (else (scan (cdr bindings)))))))

(define (evaluate-list operands env)
  (if (null? operands)
      '()
      (let ((first_ops (eval (first-operand operands) env))
	    (rest_ops (rest-operands operands)))
	(if (null? rest_ops)
	    (list first_ops)
	    (cons first_ops
		  (evaluate-list rest_ops env))))))

(defhandler apply
  (lambda (procedure operands calling-environment)
    (let ((evaluated-operands (evaluate-list operands calling-environment))
	  (has-operands (not (no-operands? operands))))
      ;(bkpt 'ha 'ha)
      (cond ((and has-operands
		  (compound-procedure? (first-operand evaluated-operands)))
	     (let ((p (first-operand evaluated-operands)))
	       (apply-primitive-procedure procedure
					  (lambda y
					    (apply p y calling-environment))
					  (rest-operands evaluated-operands))))
	    ((and has-operands (any symbolic? evaluated-operands)
		  (true? ALLOW-SELF-EVALUATING-SYMBOLS))
	     (cons (procedure-to-symbol procedure calling-environment)
		   evaluated-operands))
	    (else (apply-primitive-procedure procedure evaluated-operands)))))
  strict-primitive-procedure?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (if (not (= (length (procedure-parameters procedure))
		(length operands)))
	(error "Wrong number of operands supplied"))
    (let ((arguments
	   (map (lambda (parameter operand)
		  (evaluate-procedure-operand parameter
					      operand
					      calling-environment))
		(procedure-parameters procedure)
		operands)))
      (eval (procedure-body procedure)
	    (extend-environment
	     (map procedure-parameter-name
		  (procedure-parameters procedure))
	     arguments
	     (procedure-environment procedure)))))
  compound-procedure?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (if (= (vector-length procedure) 0) (vector)
	(let ((first_proc (vector-ref procedure 0))
	      (rest_proc (vector-tail procedure 1)))
	      (vector-append (vector (apply first_proc operands calling-environment))
		    (apply rest_proc operands calling-environment)))))
  vector?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (cons procedure (evaluate-list operands calling-environment)))
  variable?)

(define evaluate-procedure-operand
  (make-generic-operator 3
			 'evaluate-operand
			 (lambda (parameter operand environment)
			   (eval operand environment))))

(define procedure-parameter-name
  (make-generic-operator 1 'parameter-name (lambda (x) x)))
