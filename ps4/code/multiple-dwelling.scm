; multiple-dwelling probleml setup

; problem 4.3 - I had to create this method to force ambeval to interpret the
; defines, otherwise the procedure is defined in the top-level environment (not
; in our interpretter) and so it is interpretted as a primitive procedure, and
; then 'amb is not defined.
(setup)
(define (eval-fix input)
  (ambeval input
	   the-global-environment
	   (lambda (val next-alternative)
	     (display "loaded successfully!"))
	   (lambda ()
	     (display "loading failed!"))))


(eval-fix '(define (require p) (if (not p) (amb))))

(eval-fix '(define (distinct l)
  (cond ((null? l) true)
	((null? (cdr l)) true)
	((member (car l) (cdr l)) false)
	(else (distinct (cdr l))))))

(eval-fix '(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper   (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (distinct (list baker cooper)))
      (let ((fletcher (amb 1 2 3 4 5)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (distinct (list baker cooper fletcher)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(let ((miller   (amb 1 2 3 4 5)))
	  (require (distinct (list baker cooper fletcher miller)))
	  (require (> miller cooper))
	  (let ((smith    (amb 1 2 3 4 5)))
	    (require
	     (distinct (list baker cooper fletcher miller smith)))
	    (require (not (= (abs (- smith fletcher)) 1)))
	    (list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith)))))))))
