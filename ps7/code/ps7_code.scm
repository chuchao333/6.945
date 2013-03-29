(load "load")

(define (people-ify names)
  (for-each
   (lambda (name)
     (let-cells (height-cell weight-cell age-cell income-cell expenses-cell)
		(eq-put! name 'height height-cell)
		(eq-put! name 'weight weight-cell)
		(eq-put! name 'age age-cell)
		(eq-put! name 'income income-cell)
		(eq-put! name 'expenses expenses-cell)
		; someone is tall if they are over 6' (72 inches)
		(add-interval-property height-cell (make-interval 72 100) 'tall)
		; and short if under 5' (60 inches)
		(add-interval-property height-cell (make-interval 0 60) 'short)
		; TODO: these should be more intelligently set
		; someone is chubby if they are over 300 pounds
		(add-interval-property weight-cell (make-interval 300 2000) 'chubby)
		; and slender if they are under 100 pounds
		(add-interval-property weight-cell (make-interval 0 100) 'slender)
		; and healthy otherwise
		(add-interval-property weight-cell (make-interval 100 300) 'healthy)
		; someone is old if they are over 60 years
		(add-interval-property age-cell (make-interval 60 120) 'old)
		; someone is a babe if they are under 5
		(add-interval-property age-cell (make-interval 0 5) 'babe)
		; someone is a child if they are under 10
		(add-interval-property age-cell (make-interval 0 10) 'child)
		; someone is a teen if they are between 12 and 18
		(add-interval-property age-cell (make-interval 12 18) 'teen)
		; someone is a ya (young adult) if they are between 14 and 20
		(add-interval-property age-cell (make-interval 14 20) 'ya)
		; and an adult if older than 20
		(add-interval-property age-cell (make-interval 21 120) 'adult)
		; someone is rich if they make over $250k
		(add-interval-property income-cell (make-interval 250 10000) 'rich)
		; and poor if they make less than $30k
		(add-interval-property income-cell (make-interval 0 30) 'poor)
		; and middle-class otherwise
		(add-interval-property income-cell (make-interval 30 250) 'middle-class)
		; someone is a spender if they have expenses over $100k
		(add-interval-property expenses-cell (make-interval 100 10000) 'spender)
		; and a miser if they have expenses less than $10k
		(add-interval-property expenses-cell (make-interval 0 10) 'miser)
		; they're frugal if they have expenses under $40k
		(add-interval-property expenses-cell (make-interval 0 40) 'frugal)
		))
   names))