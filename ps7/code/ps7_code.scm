(load "load")

;;; Problem 7.1
(define (people-ify . names)
  (for-each
   (lambda (name)
     (let-cells
      ;; first the property cells
      (height-cell weight-cell age-cell income-cell expenses-cell
		   ;; then the indicator cells
		   lbm-cell profligate-cell miser-cell frugal-cell
		   obese-cell wealthy-cell)
      ;; Height
      (eq-put! name 'height height-cell)
      ((c:bins
	(named-ranges name
		      ;; someone is tall if they are over 6' (72 inches)
		      `(tall ,(make-interval 72 100))
		      ;; and short if under 5' (60 inches)
		      `(short ,(make-interval 0 60))))
       height-cell)

      ;; Weight
      (eq-put! name 'weight weight-cell)
      ;; TODO: these should be more intelligently set
      ((c:bins
	(named-ranges name
		      ;; someone is chubby if they are over 300 pounds
		      `(chubby ,(make-interval 300 2000))
		      ;; and slender if they are under 100 pounds
		      `(slender ,(make-interval 0 100))
		      ;; and healthy otherwise
		      `(healthy ,(make-interval 100 300))))
       weight-cell)

      ;; Age
      (eq-put! name 'age age-cell)
      ((c:bins
	(named-ranges name
		      ;; someone is old if they are over 60 years
		      `(old ,(make-interval 60 120))
		      ;; someone is a baby if they are under 5
		      `(baby ,(make-interval 0 5))
		      ;; someone is a child if they are under 10
		      `(child ,(make-interval 0 10))
		      ;; someone is a teen if they are between 12 and 18
		      `(teen ,(make-interval 12 18))
		      ;; someone is a ya (young adult) if they are between 14 and 20
		      `(ya   ,(make-interval 14 20))
		      ;; and an adult if older than 20
		      `(adult ,(make-interval 21 120))))
       age-cell)

      ;; Income
      (eq-put! name 'income income-cell)
      ((c:bins
	(named-ranges name
		      ;; someone is rich if they make over $250k
		      `(rich ,(make-interval 250 10000))
		      ;; and poor if they make less than $30k
		      `(poor ,(make-interval 0 30))
		      ;; and middle-class otherwise
		      `(middle-class ,(make-interval 30 250))))
       income-cell)

      ;; Expenses
      (eq-put! name 'expenses expenses-cell)
      ((c:bins
	(named-ranges name
		      ;; someone is a spender if they have expenses over $100k
		      `(spender ,(make-interval 100 10000))
		      ;; and a miser if they have expenses less than $30k
		      `(miser ,(make-interval 0 30))))
       expenses-cell)

      ;; Interesting cells and constraints
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
