(load "load")

;;; Problem 7.1
(define (people-ify . names)
  (for-each
   (lambda (name)
     (let-cells (height-cell weight-cell age-cell income-cell expenses-cell
			     lbm-cell profligate-cell miser-cell frugal-cell
			     obese-cell wealthy-cell)
		;; Height
		(eq-put! name 'height height-cell)
		; someone is tall if they are over 6' (72 inches)
		(add-interval-property height-cell (make-interval 72 100) 'tall)
		; and short if under 5' (60 inches)
		(add-interval-property height-cell (make-interval 0 60) 'short)

		;; Weight
		(eq-put! name 'weight weight-cell)
		; TODO: these should be more intelligently set
		; someone is chubby if they are over 300 pounds
		(add-interval-property weight-cell (make-interval 300 2000) 'chubby)
		; and slender if they are under 100 pounds
		(add-interval-property weight-cell (make-interval 0 100) 'slender)
		; and healthy otherwise
		(add-interval-property weight-cell (make-interval 100 300) 'healthy)

		;; Age
		(eq-put! name 'age age-cell)
		; someone is old if they are over 60 years
		(add-interval-property age-cell (make-interval 60 120) 'old)
		; someone is a baby if they are under 5
		(add-interval-property age-cell (make-interval 0 5) 'baby)
		; someone is a child if they are under 10
		(add-interval-property age-cell (make-interval 0 10) 'child)
		((c:bins
		  (named-ranges name
				; someone is a teen if they are between 12 and 18
				`(teen ,(make-interval 12 18))
				; someone is a ya (young adult) if they are between 14 and 20
				`(ya   ,(make-interval 14 20))))
		 age-cell)
		; and an adult if older than 20
		(add-interval-property age-cell (make-interval 21 120) 'adult)

		;; Income
		(eq-put! name 'income income-cell)
		; someone is rich if they make over $250k
		(add-interval-property income-cell (make-interval 250 10000) 'rich)
		; and poor if they make less than $30k
		(add-interval-property income-cell (make-interval 0 30) 'poor)
		; and middle-class otherwise
		(add-interval-property income-cell (make-interval 30 250) 'middle-class)
		; someone is a spender if they have expenses over $100k

		;; Expenses
		(eq-put! name 'expenses expenses-cell)
		(add-interval-property expenses-cell (make-interval 100 10000) 'spender)
		; and a miser if they have expenses less than $30k
		(add-interval-property expenses-cell (make-interval 0 30) 'miser)

		;;; Interesting cells and constraints
		;; living beyond means
		(eq-put! name 'living-beyond-means lbm-cell)
		(p:> expenses-cell income-cell lbm-cell)
		;; profligate
		(eq-put! name 'profligate profligate-cell)
		(c:and ((eq-path '(rich income)) name) lbm-cell profligate-cell)
		;; frugal
		(eq-put! name 'frugal frugal-cell)
		(c:and ((eq-path '(poor income)) name) (ce:not lbm-cell) frugal-cell)
		;; obese
		(eq-put! name 'obese obese-cell)
		(p:conditional (e:> (eq-get name 'age) 18)
			       (e:> (eq-get name 'weight)
				    (e:* (eq-get name 'height) 3))
			       (e:> (eq-get name 'weight)
				    (e:+ (e:* (eq-get name 'age) 16) 10))
			       obese-cell)
		;; wealthy
		(eq-put! name 'wealthy wealthy-cell)
		(c:and ((eq-path '(rich income)) name) (ce:not lbm-cell) wealthy-cell)

		))
   names))