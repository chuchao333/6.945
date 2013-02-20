;;; Unit tests for generic-sequences, based on descriptions from generic-specs
;;; Author Owen Derby (ocderby@csail.mit.edu)

(load "load")

(define (assert pred #!optional msg)
  (if pred
      pred
      (if (default-object? msg)
	  (error "assertion failed")
	  (error msg))))

;;; Constructors
(define v1 (sequence:construct vector? 'a 'b 'c))
(define s1 (sequence:construct string? 'a 'b 'c))
;(define s1 (sequence:construct string? #\a #\b #\c))
(define l1 (sequence:construct list? 'a 'b 'c))

(assert (equal? v1 #(a b c)) "vector construction failed")
(assert (string=? s1 "abc") "string construction failed")
(assert (equal? l1 (list 'a 'b 'c)) "list construction failed")

(assert (equal? (sequence:null vector?) #()) "non null vector")
(assert (equal? (sequence:null string?) "") "non null string")
(assert (equal? (sequence:null list?) '()) "non null list")

;;; Selecting
(assert (equal? (sequence:ref v1 1) 'b) "wrong vector element")
(assert (equal? (sequence:ref s1 1) #\b) "wrong string element")
(assert (equal? (sequence:ref l1 1) 'b) "wrong list element")

(assert (= 3 (sequence:size v1)) "wrong vector size")
(assert (= 3 (sequence:size s1)) "wrong string size")
(assert (= 3 (sequence:size l1)) "wrong list size")

(assert (eq? (sequence:type v1) vector?) "wrong vector predicate")
(assert (eq? (sequence:type s1) string?) "wrong string predicate")
(assert (eq? (sequence:type l1) list?) "wrong list predicate")

;;; Testing
(assert (sequence:null? (sequence:null vector?)) "bad null vector check")
(assert (sequence:null? (sequence:null string?)) "bad null string check")
(assert (sequence:null? (sequence:null list?)) "bad null list check")

(assert (sequence:equal? v1 #(a b c)) "vector comparison failed")
(assert (not (sequence:equal? v1 #(a a c))) "vector bad comparison failed")
(assert (sequence:equal? s1 "abc") "string comparison failed")
(assert (not (sequence:equal? s1 "aaa")) "string bad comparison failed")
(assert (sequence:equal? l1 (list 'a 'b 'c)) "list comparison failed")
(assert (not (sequence:equal? l1 '(a a c))) "vector bad comparison failed")

;;; Mutation
(let ((v2 (sequence:construct vector? 'a 'a 'c)))
  (sequence:set! v2 1 'b)
  (assert (sequence:equal? v1 v2) "vector mutation failed"))
(let ((s2 (sequence:construct string? 'a 'a 'c)))
  (sequence:set! s2 1 #\b)
  (assert (sequence:equal? s1 s2) "string mutation failed"))
(let ((l2 (sequence:construct list? 'a 'a 'c)))
  (sequence:set! l2 1 'b)
  (assert (sequence:equal? l1 l2) "list mutation failed"))

;;; Cutting and Pasting
(assert (sequence:equal? (sequence:subsequence v1 0 3)
			 v1) "vector complete subsequence failed")
(assert (sequence:equal? (sequence:subsequence v1 0 2)
			 #(a b)) "vector subsequence failed")
(assert (sequence:null? (sequence:subsequence v1 0 0))
	"vector empty subsequence failed")

(assert (sequence:equal? (sequence:subsequence s1 0 3) s1)
	"string complete subsequence failed")
(assert (sequence:equal? (sequence:subsequence s1 0 2) "ab")
	"string subsequence failed")
(assert (sequence:null? (sequence:subsequence s1 0 0))
	"string empty subsequence failed")

(assert (sequence:equal? (sequence:subsequence l1 0 3) l1)
	"list complete subsequence failed")
(assert (sequence:equal? (sequence:subsequence l1 0 2) '(a b))
	"list subsequence failed")
(assert (sequence:null? (sequence:subsequence l1 0 0))
	"list empty subsequence failed")

(assert (sequence:equal? (sequence:append v1 v1) #(a b c a b c))
	"vector append failed")
(assert (sequence:equal? (sequence:append s1 s1) "abcabc")
	"string append failed")
(assert (sequence:equal? (sequence:append l1 l1) '(a b c a b c))
	"list append failed")

;;; Iterators
(assert (sequence:equal? (sequence:generate vector? 3 (lambda (x) x))
			 #(0 1 2)) "vector:generate failed")
(assert (sequence:equal? (sequence:generate string? 3 (lambda (x) x))
			 "012") "string:generate failed")
(assert (sequence:equal? (sequence:generate list? 3 (lambda (x) x))
			 '(0 1 2)) "list:generate failed")

(assert (sequence:equal? (sequence:map (lambda (x y z) (+ x y z))
				       #(1 2) #(1 2) #(1 2))
			 #(3 6))
	"vector:map failed")
(assert (sequence:equal? (sequence:map (lambda (x y z) (string x y z))
				       "ab" "  " "cd")
			 "a cb d")
	"string:map filed")


(assert (sequence:equal? (let ((v #(1 2 3)))
			   (sequence:for-each (lambda (i j k)
						(sequence:set! v i (* j j)))
					      #(0 1 2) #(3 2 1) #(3 2 1))
			   v)
			 #(9 4 1))
	"vector:for-each failed")

;;; Filtration and Search
(assert (sequence:equal? (sequence:filter #(1 2 3) odd?)
			 #(1 3)) "filtering failed")

(assert (= (sequence:get-index #(1 2 3) odd?) 0) "get-index failed")

(assert (= (sequence:get-element #(1 2 3) odd?) 1) "get-element failed")

;;; Accumulation
(assert (sequence:equal? (sequence:fold-right
			  list 'end '(1 2 3))
			 '(1 (2 (3 end)))) "fold-right failed")

(assert (sequence:equal? (sequence:fold-left
			  list 'start '(1 2 3))
			 '(((start 1) 2) 3)) "fold-right failed")

; Problem 2.2 - generalizations

;;; generalized equal?
(assert (sequence:equal-ta? #(1 2 3) '(1 2 3)) "equal-ta_1 failed")
;(assert (sequence:equal-ta? #(#\1 #\2 #\3) "123") "equal-ta_1b failed")
;(assert (sequence:equal-ta? '(#\1 #\2 #\3) "123") "equal-ta_1c failed")
(assert (sequence:equal-ta? '(1 #(2 3) 4) #(1 (2 3) 4)) "equal-ta_2 failed")
(assert (not
	 (sequence:equal-ta? '(1 #(2 3) 4) #(1 (3 2) 4))) "equal-ta_2b failed")
;(assert (not
;	 (sequence:equal-ta? '(1 #(2 3) 4) '(1 "23" 4))) "equal-ta_3 failed")
;(assert (sequence:equal-ta? '(1 #(#\2 #\3) 4) '(1 "23" 4)) "equal-ta_3b failed")

;;; generalized append
(define v2 (sequence:construct vector? 1 2 3))
;(define s2 (sequence:construct string? 1 2 3))
(define l2 (sequence:construct list? ))

(assert (sequence:equal? (sequence:append v1 l1)
			 #(a b c a b c)) "vector-general append failed")
;(assert (sequence:equal? (sequence:append s2 l2 v2)
;			 "abcabcabc") "string-general append failed")
(assert (sequence:equal? (sequence:append l1 v1)
			 '(a b c a b c)) "list-general append failed")

;;; generalized map
(assert (sequence:equal? (sequence:map (lambda (x y z) (+ x y z))
				       #(1 2) '(1 2) #(1 2))
			 #(3 6))
	"general map failed")

;;; generalized for-each
(assert (sequence:equal? (let ((v #(1 2 3)))
			   (sequence:for-each (lambda (i j k)
						(sequence:set! v i (* j j)))
					      #(0 1 2) '(3 2 1) "321")
			   v)
			 #(9 4 1))
	"general for-each failed")
