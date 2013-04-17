(load "load")

;;; Problem 8.2B: Redefine lazy-fringe
(define (lazy-fringe subtree)
  (define (walk subtree ans)
    (cond ((pair? subtree)
	   (walk (car subtree)
		 (lambda () (walk (cdr subtree) ans))))
	  ((null? subtree) (ans))
	  (else (cons-stream subtree (ans)))))
  (walk subtree (lambda () the-empty-stream)))
