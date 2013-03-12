
;;; Problem 5.2
;; Given tests
((match:->combinators '(?:choice a b (? x) c)) 'z '() (lambda (d) `(succeed ,d)))
; (succeed ((x z)))
((match:->combinators `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
 '(z z) '() (lambda (d) `(succeed ,d)))
; (succeed ((y z)))
((match:->combinators `(?:choice b (? x ,symbol?)))
 'b '() (lambda (x) (pp `(succeed ,x)) #f))
; (succeed ())
; (succeed ((x b)))
; Value: #f
;; Additional tests
((match:->combinators '(?:choice)) 'z '() (lambda (d) `(succeed ,d)))
; Value: #f
((match:->combinators '(?:choice)) '() '() (lambda (d) `(succeed ,d)))
; Value: (succeed ())
((match:->combinators '(?:choice a b)) 'z '() (lambda (d) `(succeed ,d)))
; Value: #f
;; test list with choices
((match:->combinators '(a (?:choice (? x) b (?? y)) (?? x) (?:choice c (? x) )))
 '(a b b c) '() (lambda (dict) (pp `(succeed ,dict)) #f))
; (succeed ((x (b))))
; (succeed ((x (b c))))
; (succeed ((x (b b)) (y ())))
; (succeed ((x (b b c)) (y ())))
; (succeed ((x (b)) (y (b))))
; (succeed ((x (b c)) (y (b))))
; (succeed ((x ()) (y (b b))))
; (succeed ((x (c)) (y (b b))))
; (succeed ((x ()) (y (b b c))))
; Value: #f
;; test choice of lists
((match:->combinators '(?:choice (a (?? x) c) ((? x) (?? y) c))) '(a b b c) '() (lambda (dict) (pp `(succeed ,dict)) #f))
; (succeed ((x (b b))))
; (succeed ((y (b b)) (x a)))
; Value: #f
((match:->combinators '(?:choice a b (? x) c)) 'z '() (lambda (d) `(succeed ,d)))

;;; Problem 5.3

((match:->combinators '(?:pletrec () (?:choice a b (? x) c))) 'z '() (lambda (d) `(succeed ,d)))

((match:->combinators '(?? x)) '(a a) '() (lambda (dict n) (pp `(succeed ,dict)) #f))

((match:->combinators '(?:pletrec ((foo (?? x))) (?:ref foo))) '(a a) '() (lambda (dict) (pp `(succeed ,dict)) #f))

((match:->combinators '(?:pletrec ((foo (?? x)) (bar (?? y))) (?:choice (?:ref foo) (?:ref bar)))) '(a a) '() (lambda (dict) (pp `(succeed ,dict)) #f))

((match:->combinators '(?:pletrec ((odd-even-etc (?:choice () (1 (?:ref even-odd-etc))))
				   (even-odd-etc (?:choice () (2 (?:ref odd-even-etc)))))
				  (?:ref odd-even-etc)))
 '(1 (2 (1 (2 (1 ())))))
 (lambda (d) `(succeed ,d)))
