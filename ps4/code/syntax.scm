;;; -*- Mode:Scheme -*-

(declare (usual-integrations))

;;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp #t)
      (eq? exp #f)
      (string? exp)))	; Our prompt (viz., "EVAL==> ") is a string.

;;; Variables

(define (variable? exp) (symbol? exp))

(define (same-variable? var1 var2) (eq? var1 var2))  ;; Nice abstraction

;;; Special forms (in general)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;;; Quotations

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation quot) (cadr quot))

;;; Assignment--- SET!

(define (assignment? exp) (tagged-list? exp 'set!))
(define (permanent-assignment? exp) (tagged-list? exp 'set!!))

(define (assignment-variable assn) (cadr  assn))
(define (assignment-value    assn) (caddr assn))

;;; Definitions

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo      ...)
      (cadr  defn)
      (caadr defn)))				;;   (DEFINE (foo ...) ...)

(define (definition-value defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo        ...)
      (caddr defn)
      (cons 'lambda				;;   (DEFINE (foo p...) b...)
            (cons (cdadr defn)			;; = (DEFINE  foo
                  (cddr  defn)))))		;;     (LAMBDA (p...) b...))

;;; LAMBDA expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp)
  (let ((full-body (cddr lambda-exp)))
    (sequence->begin full-body)))


(define declaration? pair?)

(define (parameter-name var-decl)
  (if (pair? var-decl)
      (car var-decl)
      var-decl))

(define (lazy? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (not (memq 'memo (cdr var-decl)))))

(define (lazy-memo? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (memq 'memo (cdr var-decl))))

(define (sequence->begin seq)
  (cond ((null? seq) seq)
	((null? (cdr seq)) (car seq))
	((begin? (car seq)) seq)
	(else (make-begin seq))))

(define (make-begin exp) (cons 'begin exp))

;;; If conditionals

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'IF pred conseq alternative))


;;; COND Conditionals

(define (cond? exp) (tagged-list? exp 'cond))

(define (clauses cndl) (cdr cndl))
(define (no-clauses? clauses) (null? clauses))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (else-clause? clause) (eq? (predicate clause) 'else))

(define (predicate clause) (car clause))

(define (actions clause)
  (sequence->begin (cdr clause)))

(define (cond->if cond-exp)
  (define (expand clauses)
    (cond ((no-clauses? clauses)
	   (list 'error "COND: no values matched"))
	  ((else-clause? (car clauses))
	   (if (no-clauses? (cdr clauses))
	       (actions (car clauses))
	       (error "else clause isn't last -- INTERP" exp)))
	  (else
	   (make-if (predicate (car clauses))
		    (actions (car clauses))
		    (expand (cdr clauses))))))
  (expand (clauses cond-exp)))


;;; BEGIN expressions (a.k.a. sequences)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define no-more-exps? null?)		; for non-tail-recursive vers.

;;; LET expressions

(define (let? exp) (tagged-list? exp 'let))
(define (let-bound-variables let-exp)
  (map car (cadr let-exp)))
(define (let-values let-exp) (map cadr (cadr let-exp)))
(define (let-body let-exp) (sequence->begin (cddr let-exp)))
(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
	(values (let-values let-exp))
	(body (let-body let-exp)))
    (cons (list 'LAMBDA names body)
	  values)))

;;; Procedure applications -- NO-ARGS? and LAST-OPERAND? added

(define (application? exp)
  (pair? exp))

(define (no-args? exp)				;; Added for tail recursion
  (and (pair? exp)
       (null? (cdr exp))))

(define (args-application? exp)			;; Changed from 5.2.1
  (and (pair? exp)
       (not (null? (cdr exp)))))


(define (operator app) (car app))
(define (operands app) (cdr app))

(define (last-operand? args)			;; Added for tail recursion
  (null? (cdr args)))

(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))

;;; Another special form that will be needed later.

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))

(define (amb-alternatives exp) (cdr exp))

;;; Infix notation handling

(define (count-lp exp)
  (length (string-search-all "(" exp)))
(define (count-rp exp)
  (length (string-search-all ")" exp)))
(define (well-balanced? exp) (= (count-lp exp) (count-rp exp)))

(define (has? op exp)
  (not (eq? (string-search-forward op exp) false)))
(define (first-exp op exp)
  (let ((op_idx (string-search-forward op exp)))
    (string-head exp op_idx)))
(define (rest-exp op exp)
  (let ((op_idx (string-search-forward op exp)))
    (string-tail exp (+ op_idx 1))))

; in general, if we only look at +/- ops, and assume parens have been
; simplified, we have expression A-B+/-C which should turn into A+B'+/-C, where
; B' is (-1*B) and we can recurse
(define (normalize-neg exp)
  (let* ((fe (first-exp "-" exp)) (ne (rest-exp "-" exp))
	 (ridx (string-find-next-char-in-set ne (char-set #\+ #\-)))
	 (neg (if ridx
		   (list '* -1 (parse-exp (string-head ne ridx)))
		   (list '* -1 (parse-exp ne)))))
    (cond ((and (string-null? fe) (not ridx)) ;no A subexp or C subexp
	   neg)
	  ((and (string-null? fe) ridx) ;no A subexp, but yes to C
	   (list '+ neg (parse-exp (string-tail ne ridx))))
	  ((and (not (string-null? fe)) (not ridx)) ;A, but no C
	   (list '+ (parse-exp fe) neg))
	  ((and (not (string-null? fe)) ridx) ; we have both A and C
	   (list '+ (parse-exp fe) neg (parse-exp (string-tail ne ridx))))
	  (else (error "huh??" exp)))))

(define (normalize-div exp)
  (let* ((fe (first-exp "/" exp)) (ne (rest-exp "/" exp))
	 (ridx (string-find-next-char-in-set ne (char-set #\/ #\*)))
	 (div (if ridx
		   (list '/ 1 (parse-exp (string-head ne ridx)))
		   (list '/ 1 (parse-exp ne)))))
    (cond ((string-null? fe) (error "no numerator in divide: " exp))
	  ((not ridx) (list '* (parse-exp fe) div))
	  (ridx (list '* (parse-exp fe) div (parse-exp (string-tail ne (+ 1 ridx)))))
	  (else "impossible!"))))

(define (parse-exp exp)
  (cond
   ((has? "-" exp) (normalize-neg exp))
   ((has? "+" exp) (let ((fe (first-exp "+" exp)))
		     (if (string-null? fe)
			 (parse-exp (rest-exp "+" exp))
			 (list '+ (parse-exp (first-exp "+" exp)) (parse-exp (rest-exp "+" exp))))))
   ((has? "/" exp) (normalize-div exp))
   ((has? "*" exp) (list '* (parse-exp (first-exp "*" exp)) (parse-exp (rest-exp "*" exp))))
   ((has? "^" exp) (list 'expt (parse-exp (first-exp "^" exp)) (parse-exp (rest-exp "^" exp))))
   ((string->number exp) (string->number exp))
   (else (string->symbol exp))
   ))
;
;(define (find-sub-exp exp)
;  (define (find-sub-exp-end subexp count)
;    (let ((fc (string-head subexp 1)))
;      (cond ((string=? fc "(") (find-sub-exp-end (string-tail subexp 1) (+ count 1)))
;	    ((string=? fc ")")
;    (if (string-null? subexp)
;	(if (= count
;
;
;
;(define (infix->prefix exp)
;  (let ((fc (string-head exp 1)))
;    (if (string=? fc "(")
