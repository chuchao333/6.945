;; extend our evaluator to parse strings, substituting values from the
;; environment for "$var" type patterns, where "var" is looked up in the current
;; environment

(defhandler eval
  (lambda (expression environment)
    (let lookup ((str expression))
      (if (string-null? str) ""
	  (let ((r (string-search-forward "$" str)))
	    (if (false? r)
		str
		(let* ((s (substring-search-forward " " str r
						    (string-length str)))
		       (t (if (false? s) (string-length str)
			      s))
		       (sub (substring str (+ r 1) t))
		       (val (if (false? (string->number sub))
				(let ((out (eval (string->symbol sub) environment)))
				  (if (string? out) out
				      (write-to-string out)))
				sub)))
		  ;(bkpt 'ah 'ha)
		  (if (false? s) (string-append (string-head str r) val)
		      (string-append (string-head str r) val
				   (lookup (string-tail str t))))))))))
  string?)

(define la "sing to me!!")
"I say $la $la $la"
; ==> "I say sing to me!! sing to me!! sing to me!!"

;; This is particularly useful for error messages/debug prints
(define (foo x)
  (if (< x 0) (display "$x is less than 0!")
      (display "$x is greater than 0!")))
(foo -3)
; ==> "-3 is less than 0!"
(foo 3)
; ==> "3 is greater than 0!"