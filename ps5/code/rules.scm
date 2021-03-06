;;;; File:  rules.scm -- Some sample algebraic simplification rules

(define algebra-1
  (rule-simplifier
   (list
    ;; Associative law of addition
    (rule '(+ (? a) (+ (? b) (? c)))
	  `(+ (+ ,a ,b) ,c))

    ;; Commutative law of multiplication
    (rule '(* (? b) (? a))
	  (and (expr<? a b)
	       `(* ,a ,b)))

    ;; Distributive law of multiplication over addition
    (rule '(* (? a) (+ (? b) (? c)))
	  `(+ (* ,a ,b) (* ,a ,c))) )))

(define (list<? x y)
  (let ((nx (length x)) (ny (length y)))
    (cond ((< nx ny) #t)
	  ((> nx ny) #f)
	  (else
	   (let lp ((x x) (y y))
	     (cond ((null? x) #f)	; same
		   ((expr<? (car x) (car y)) #t)
		   ((expr<? (car y) (car x)) #f)
		   (else (lp (cdr x) (cdr y)))))))))

(define expr<?
  (make-entity
   (lambda (self x y)
     (let per-type ((types (entity-extra self)))
       (if (null? types)
	   (error "Unknown expression type -- expr<?" x y)
	   (let ((predicate? (caar types))
		 (comparator (cdar types)))
	     (cond ((predicate? x)
		    (if (predicate? y)
			(comparator x y)
			#t))
		   ((predicate? y) #f)
		   (else (per-type (cdr types))))))))
   `((,null?   . ,(lambda (x y) #f))
     (,number? . ,<)
     (,symbol? . ,symbol<?)
     (,list?   . ,list<?))))
#|
 (algebra-1 '(* (+ y (+ z w)) x))
 ;Value: (+ (+ (* x y) (* x z)) (* w x))
|#

(define algebra-2
  (rule-simplifier
   (list

    ;; Sums

    (rule `(+ (? a)) a)

    (rule `(+ (?? a) (+ (?? b)) (?? c))
	  `(+ ,@a ,@b ,@c))

    (rule `(+ (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(+ ,@a ,x ,y ,@b)))


    ;; Products

    (rule `(* (? a)) a)

    (rule `(* (?? a) (* (?? b)) (?? c))
	  `(* ,@a ,@b ,@c))

    (rule `(* (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(* ,@a ,x ,y ,@b)))


    ;; Distributive law

    (rule `(* (?? a) (+ (?? b)) (?? c))
	  `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


    ;; Numerical simplifications below

    (rule `(+ 0 (?? x)) `(+ ,@x))

    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
	  `(+ ,(+ x y) ,@z))


    (rule `(* 0 (?? x)) 0)

    (rule `(* 1 (?? x)) `(* ,@x))

    (rule `(* (? x ,number?) (? y ,number?) (?? z))
	  `(* ,(* x y) ,@z))

    ;; Collect like terms

    ; x+x => 2x
    (rule `(+ (?? a) (? x ,symbol?) (? x) (?? c))
	  `(+ ,@a (* 2 ,x) ,@c))

    ; x+2x => 3x
    (rule `(+ (?? a) (? x ,symbol?) (?? b) (* (? y ,number?) (? x ,symbol?)) (?? c))
	  `(+ ,@a ,@b (* ,(+ 1 y) ,x) ,@c))

    ; xy+xy => 2xy
    (rule `(+ (?? a) (* (? x ,symbol?) (?? w)) (* (? x) (?? w)) (?? c))
	  `(+ ,@a (* 2 ,x ,@w) ,@c))

    ; xy+2xy => 3xy
    (rule `(+ (?? a) (* (? x ,symbol?) (?? w)) (* (? y ,number?) (? x) (?? w)) (?? c))
	  `(+ ,@a (* ,(+ 1 y) ,x ,@w) ,@c))

    ; 2x+3x = > 5x
    (rule `(+ (?? a) (* (? y ,number?) (?? x)) (* (? z ,number?) (?? x)) (?? c))
	  `(+ ,@a (* ,(+ y z) ,@x) ,@c))

    )))

#|
 (algebra-2 '(* (+ y (+ z w)) x))
 ;Value: (+ (* w x) (* x y) (* x z))

 (algebra-2 '(+ (* 3 (+ x 1)) -3))
 ;Value: (* 3 x)
|#
