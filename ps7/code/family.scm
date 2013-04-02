;;;; A Small Financial Example

;;; First, we need a small database mechanism
;;;  Parent and child here do not refer to biological
;;;  things, but rather the relationships of parts
;;;  of a database.

(define (add-branch! parent child name)
  (eq-put! parent name child)
  (eq-put! child 'parent parent)
  (eq-put! child 'given-name name)
  'done)

(define (name-of thing)
  (let ((n (eq-get thing 'given-name)))
    (if n
	(let ((p (eq-get thing 'parent)))
	  (if p
	      (cons n (name-of p))
	      (list n)))
	(list (name thing)))))

;;; e.g. (thing-of Gaggle-salary gross-income Ben)

(define (thing-of name-path)
  (let lp ((path name-path))
    (cond ((= (length path) 1) (car path))
	  (else
	   (eq-get (lp (cdr path))
		   (car path))))))



;;; A financial entity has three cells

(define (make-financial-entity entity)
  (eq-put! entity 'kind-of-entity 'financial)

  (let-cells (gross-income expenses net-income)

    (add-branch! entity gross-income 'gross-income)
    (add-branch! entity net-income 'net-income)
    (add-branch! entity expenses 'expenses)

    (c:+ expenses net-income gross-income)
    'done
    ))

(define (financial-entity? thing)
  (eq? (eq-get thing 'kind-of-entity) 'financial))

(define (gross-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'gross-income))

(define (net-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'net-income))

(define (expenses entity)
  (assert (financial-entity? entity))
  (eq-get entity 'expenses))

(define (breakdown sum-node . part-names)
  (for-each (lambda (part-name)
	      (let-cell part
			(add-branch! sum-node part part-name)))
	    part-names)
  (let lp ((sum-node sum-node)
	   (part-names part-names))
    (cond ((= (length part-names) 2)
	   (c:+ (eq-get sum-node (car part-names))
		(eq-get sum-node (cadr part-names))
		sum-node)
	   'done)
	  (else
	   (let-cell sub-sum-node
		     (lp sub-sum-node (cdr part-names))
		     (c:+ (eq-get sum-node (car part-names))
			  sub-sum-node
			  sum-node))
	   'done))))

(define (combine-financial-entities compound . parts)
  (assert (every financial-entity? parts) "every part should be a financial entity")
  (assert (financial-entity? compound) "compound should be a financial entity")
  (define (combiner p1 p2 p3)
    (c:+ (gross-income p1) (gross-income p2) (gross-income p3))
    (c:+ (net-income p1) (net-income p2) (net-income p3))
    (c:+ (expenses p1) (expenses p2) (expenses p3))
    'done)
  (let ((p1 (car parts)) (p2 (cadr parts)) (rest (cdr parts)))
    (cond ((= (length parts) 2)
	   (combiner p1 p2 compound))
	  (else
	   (let-cell sub-compound
		     (make-financial-entity sub-compound)
		     (apply combine-financial-entities (cons sub-compound rest))
		     (combiner p1 sub-compound compound)))
	  )))

;#|
(initialize-scheduler)

(make-financial-entity 'Alyssa)
(make-financial-entity 'Ben)

;;; Ben and Alyssa are married
(make-financial-entity 'Ben-Alyssa)
(combine-financial-entities 'Ben-Alyssa 'Ben 'Alyssa)

;;; Ben and Alyssa file income tax jointly
(tell! (gross-income 'Ben-Alyssa) 427000 'IRS)

;;; Ben works at Gaggle as a software engineer.
(breakdown (gross-income 'Ben) 'Gaggle-salary 'investments)

;;; He gets paid alot to make good apps.
(tell! (thing-of '(Gaggle-salary gross-income Ben)) 200000 'Gaggle)

;;; Alyssa works as a PhD biochemist in big pharma.
(breakdown (gross-income 'Alyssa) 'GeneScam-salary 'investments)

;;; Biochemists are paid poorly.
(tell! (thing-of '(GeneScam-salary gross-income Alyssa)) 70000 'GeneScam)

(tell! (thing-of '(investments gross-income Alyssa))
       (make-interval 30000 40000) 'Alyssa)

(inquire (thing-of '(investments gross-income Ben)))
;Value: #(supported #[interval 117000 127000] (gaggle genescam alyssa irs))

;;; Ben is a tightwad
(tell! (thing-of '(expenses Ben)) (make-interval 10000 20000) 'Ben)

(inquire (thing-of '(net-income Ben)))
;Value: #(supported #[interval 297000 317000] (ben genescam alyssa irs))

;;; But Alyssa is not cheap.  She likes luxury.
(tell! (thing-of '(expenses Alyssa)) (make-interval 200000 215000) 'Alyssa)

(inquire (thing-of '(net-income Alyssa)))
;Value: #(supported #[interval -115000 -90000] (alyssa genescam))

;;; But they are doing OK anyway!
(inquire (thing-of '(net-income Ben-Alyssa)))
;Value: #(supported #[interval 192000 217000] (ben alyssa irs))

;;; Notice that this conclusion does not depend on the details, such
;;; as Gaggle or GeneScam!

;;; Problem 7.3: added entities and constraints
(make-financial-entity 'Harry)
(make-financial-entity 'Eva)

;;; Harry and Eva are married
(make-financial-entity 'Harry-Eva)
(combine-financial-entities 'Harry-Eva 'Harry 'Eva)

;;; Harry and Eva file income tax jointly
(tell! (gross-income 'Harry-Eva) 220000 'IRS)

;;; Harry is un-employed, but has some investments
(breakdown (gross-income 'Harry) 'salary 'investments)
(tell! (thing-of '(salary gross-income Harry)) 0 'unemployed)

;;; Eva works as a barista, but doesn't make much (varies with tips)
(breakdown (gross-income 'Eva) 'Farducks-salary 'investments)
(tell! (thing-of '(Farducks-salary gross-income Eva))
       (make-interval 30000 40000) 'Farducks)
;;; Her inheritance helps
(tell! (thing-of '(investments gross-income Eva))
       (make-interval 60000 75000) 'Eva)

;;; Both live like hermits
(tell! (expenses 'Harry) 20000 'Harry)
(tell! (expenses 'Eva) 30000 'Eva)

;;; Joe is not married
(make-financial-entity 'Joe)

;;; Joe is also a barista at Farducks, but has no investments
(tell! (gross-income 'Joe)
       (make-interval 35000 50000) 'Farducks)

;;; Joe has has some expenses
(tell! (gross-income 'Joe) 45000 'IRS)

;;; Joe is an attrocious spender
(tell! (expenses 'Joe) (make-interval 45000 75000) 'Joe)

;;; All men belong to the same golf club, the Bits
(make-financial-entity 'Bits)

;;; Income for the group comes from dues and investments
(breakdown (gross-income 'Bits) 'dues 'investments)

;;; The Bits investments return a sizable sum

(tell! (thing-of '(investments gross-income Bits))
       (make-interval 60000 75000) 'investments)

;;; All men pay the same dues
(breakdown (thing-of '(expenses Ben)) 'dues 'etc)
(breakdown (thing-of '(expenses Harry)) 'dues 'etc)
(breakdown (thing-of '(expenses Joe)) 'dues 'etc)
(ce:== (thing-of '(dues expenses Ben))
       (thing-of '(dues expenses Harry))
       (thing-of '(dues expenses Joe)))
; need to explicitly state this equivalence
(c:* (thing-of '(dues expenses Ben)) 3
     (thing-of '(dues gross-income Bits)))

;;; The club only has those 3 members
(c:+ (thing-of '(dues expenses Ben))
     (ce:+ (thing-of '(dues expenses Harry))
	  (thing-of '(dues expenses Joe)))
     (thing-of '(dues gross-income Bits)))

;;; But maintaining a golf course is expensive!
(tell! (expenses 'Bits) (make-interval 120000 170000) 'Bits)

;;; Dues are approximately set by net difference of operating expenses
(c:== (ce:+ (make-interval -5000 5000) (gross-income 'Bits))
      (expenses 'Bits))

;|#
