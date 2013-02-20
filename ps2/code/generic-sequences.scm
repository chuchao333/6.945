;;;;    Generic sequence operator definitions

;;; First we declare the operators we want to be generic.
;;;  Each declaration specifies the arity (number of arguments)
;;;  and the default operation, if necessary.

(define sequence:null (make-generic-operator 1 "sequence:null"))


(define sequence:ref (make-generic-operator 2 "sequence:ref"))

(define sequence:size (make-generic-operator 1 "sequence:size"))

(define sequence:type (make-generic-operator 1 "sequence:type"))

(define sequence:null? (make-generic-operator 1 "sequence:null?"))

(define sequence:equal? (make-generic-operator 2 "sequence:equal?"))

(define sequence:set! (make-generic-operator 3 "sequence:set!"))

(define sequence:subsequence (make-generic-operator 3 "sequence:subsequence"))


;;; sequence:append takes multiple arguments.  It is defined in terms
;;; of a generic binary append that takes two sequences.

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (let ((type? (sequence:type (car sequences))))
    (if (not (for-all? (cdr sequences) type?))
	(error "All sequences for append must be of the same type"
	       sequences))
    (fold-right generic:binary-append
		(sequence:null (sequence:type (car sequences)))
		sequences)))

(define generic:binary-append
  (make-generic-operator 2 "generic:binary-append"))

;;; Implementations of the generic operators.

(define (any? x) #t)
(define (constant val) (lambda x val))
(define (is-exactly val) (lambda (x) (eq? x val)))

(defhandler sequence:null (constant "")    (is-exactly string?))
(defhandler sequence:null (constant '())   (is-exactly list?))
(defhandler sequence:null (constant #())   (is-exactly vector?))

(defhandler sequence:ref string-ref         string? exact-integer?)
(defhandler sequence:ref list-ref           list?   exact-integer?)
(defhandler sequence:ref vector-ref         vector? exact-integer?)

(defhandler sequence:size string-length     string?)
(defhandler sequence:size length            list?)
(defhandler sequence:size vector-length     vector?)

(defhandler sequence:type (constant string?)     string?)
(defhandler sequence:type (constant list?)       list?)
(defhandler sequence:type (constant vector?)     vector?)


(define (vector-null? v) (= (vector-length v) 0))

(defhandler sequence:null? string-null?     string?)
(defhandler sequence:null? null?            list?)
(defhandler sequence:null? vector-null?     vector?)


;;; To assign to the ith element of a list:

(define (list-set! list i val)
  (cond ((null? list)
	 (error "List does not have enough elements" i))
	((= i 0) (set-car! list val))
	(else (list-set! (cdr list) (- i 1) val))))

(defhandler sequence:set! string-set!   string? exact-integer? any?)
(defhandler sequence:set! list-set!     list?   exact-integer? any?)
(defhandler sequence:set! vector-set!   vector? exact-integer? any?)


(defhandler sequence:subsequence
		  substring
		  string?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  sublist
		  list?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  subvector
		  vector?  exact-integer?  exact-integer?)


(define (vector-append v1 v2)
  (let ((n1 (vector-length v1))
	(n2 (vector-length v2)))
    (make-initialized-vector (+ n1 n2)
			     (lambda (i)
			       (if (< i n1)
				   (vector-ref v1 i)
				   (vector-ref v2 (- i n1)))))))

(defhandler generic:binary-append string-append  string? string?)
(defhandler generic:binary-append append         list?   list?)
(defhandler generic:binary-append vector-append  vector? vector?)

; problem 2.1 implementations

(define (sequence:car seq) (sequence:ref seq 0))
(define (sequence:cdr seq) (sequence:subsequence seq 1 (sequence:size seq)))

(define generic:binary-construct
  (make-generic-operator 2 "generic:binary-construct"))

(define (sequence:construct type . args)
  (if (null? args)
      (sequence:null type)
      (fold-right generic:binary-construct
		  (sequence:null type) args)))

(define (binary-append-wrapper proc) (lambda (x y) (generic:binary-append (proc x) y)))
(define (unquote-func f) (lambda (x) (f `,x)))

(defhandler generic:binary-construct
  (binary-append-wrapper (unquote-func string)) any? string?)
(defhandler generic:binary-construct
  (binary-append-wrapper (unquote-func list)) any? list?)
(defhandler generic:binary-construct
  (binary-append-wrapper (unquote-func vector)) any? vector?)

(defhandler sequence:equal? equal? vector? vector?)
(defhandler sequence:equal? string=? string? string?)
(defhandler sequence:equal? equal? list? list?)

(define (sequence:generate type n func)
  (do ((seq (sequence:null type))
       (i 0 (+ i 1)))
      ((= i n) seq)
    (set! seq (sequence:append seq (sequence:construct type (func i))))))

(define (pull sequences i)
	       (if (= (length sequences) 1)
		   (list (sequence:ref (car sequences) i))
		   (append (list (sequence:ref (car sequences) i)) (pull (cdr sequences) i))))

(define (sequence:map func . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for map"))
  (let ((type? (sequence:type (car sequences)))
	(n (sequence:size (car sequences))))
;    (if (not (for-all? (cdr sequences) type?))
;	(error "All sequences for map must be of the same type"
;	       sequences))
    (if (not (for-all? (cdr sequences) (lambda (x) (= (sequence:size x) n))))
	(error "All sequences for map must be of the same length"
	       sequences))
    (do ((seq (sequence:null type?))
	 (i 0 (+ i 1)))
	((= i n) seq)
      (set! seq
	    (sequence:append seq
			     (sequence:construct type?
						 (apply func (pull sequences i))))))))

(define (sequence:for-each func . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for map"))
  (let ((type? (sequence:type (car sequences)))
	(n (sequence:size (car sequences))))
;    (if (not (for-all? (cdr sequences) type?))
;	(error "All sequences for for-each must be of the same type"
;	       sequences))
    (if (not (for-all? (cdr sequences) (lambda (x) (= (sequence:size x) n))))
	(error "All sequences for for-each must be of the same length"
	       sequences))
    (do ((i 0 (+ i 1)))
	((= i n))
      (apply func (pull sequences i)))))

(define (sequence:filter seq pred)
  (let ((arg (sequence:car seq))
	(type? (sequence:type seq)))
    (if (= (sequence:size seq) 1)
	(if (pred arg)
	    (sequence:construct type? arg)
	    (sequence:null type?))
	(let ((ret (sequence:filter (sequence:cdr seq) pred)))
	  (if (pred arg)
	      (sequence:append (sequence:construct type? arg) ret)
	      ret)))))

(define (sequence:get-index seq pred)
  (let ((n (sequence:size seq)))
    (let loop ((i 0))
      (if (= i n)
	  #f
	  (if (pred (sequence:ref seq i))
	      i
	      (loop (+ i 1)))))))

(define (sequence:get-element seq pred)
  (let ((idx (sequence:get-index seq pred)))
    (if (eq? idx #f)
	#f
	(sequence:ref seq idx))))

(define (sequence:fold-right func init seq)
  (cond ((sequence:null? seq)
	 (error "sequence:fold-right sequence can't be null!"))
	((= (sequence:size seq) 1) (func (sequence:car seq) init))
	(else (func (sequence:car seq)
		    (sequence:fold-right func init (sequence:cdr seq))))))

(define (sequence:fold-left func init seq)
  (cond ((sequence:null? seq)
	 (error "sequence:fold-left sequence can't be null!"))
	((= (sequence:size seq) 1) (func init (sequence:car seq)))
	(else (let ((n (sequence:size seq)))
		(func (sequence:fold-left func init
					  (sequence:subsequence seq 0 (- n 1)))
		      (sequence:ref seq (- n 1)))))))

; Problem 2.2 generalizations

;(define (sequence? x) (or (string? x) (vector? x) (list? x)))
(define (mixable-sequence? x) (or (vector? x) (list? x)))

(define (sequence:equal-ta? seq-1 seq-2)
  (cond ((sequence:null? seq-1) (sequence:null? seq-2))
	((sequence:null? seq-2) (sequence:null? seq-1))
	(else
	 (let ((n1 (sequence:size seq-1))
	       (n2 (sequence:size seq-2))
	       (arg1 (sequence:car seq-1))
	       (arg2 (sequence:car seq-2))
	       (rest1 (sequence:cdr seq-1))
	       (rest2 (sequence:cdr seq-2)))
	   (cond ((not (= n1 n2)) #f)
		 ((and (mixable-sequence? arg1) (mixable-sequence? arg2))
		  (and (sequence:equal-ta? arg1 arg2)
		       (sequence:equal-ta? rest1 rest2)))
		 ((and (not (mixable-sequence? arg1)) (not (mixable-sequence? arg2)))
		  (and (equal? arg1 arg2)
		       (sequence:equal-ta? rest1 rest2)))
		 (else #f))))))


;(define (compose-1st-arg f g) (lambda (x y) (f (g x) y)))
(define (compose-2nd-arg f g) (lambda (x y) (f x (g y))))

; default list->string doesn't handle quoted constants...
;(define (list2string l) (apply string (map (lambda (x) (string (eval x user-initial-environment))) l)))
; mixing with strings sucks!
;(define (vector->string v) (list2string (vector->list v)))
;(define (string->vector s) (list->vector (string->list s)))

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (let ((type? (sequence:type (car sequences))))
    (fold-right generic:binary-append
		(sequence:null (sequence:type (car sequences)))
		sequences)))


(defhandler generic:binary-append (compose-2nd-arg vector-append list->vector)
  vector? list?)
(defhandler generic:binary-append (compose-2nd-arg append vector->list)
  list? vector?)

(define (sequence:map func . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for map"))
  (let ((type? (sequence:type (car sequences)))
	(n (sequence:size (car sequences))))
    (if (not (for-all? (cdr sequences) (lambda (x) (= (sequence:size x) n))))
	(error "All sequences for map must be of the same length"
	       sequences))
    (do ((seq (sequence:null type?))
	 (i 0 (+ i 1)))
	((= i n) seq)
      (set! seq
	    (sequence:append seq
			     (sequence:construct type?
						 (apply func (pull sequences i))))))))

(define (sequence:for-each func . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for map"))
  (let ((type? (sequence:type (car sequences)))
	(n (sequence:size (car sequences))))
    (if (not (for-all? (cdr sequences) (lambda (x) (= (sequence:size x) n))))
	(error "All sequences for map must be of the same length"
	       sequences))
    (do ((i 0 (+ i 1)))
	((= i n))
      (apply func (pull sequences i)))))


; problem 2.5.C implementation

(define (list<? list-1 list-2)
  (let ((len-1 (length list-1)) (len-2 (length list-2)))
    (cond ((< len-1 len-2) #t)
          ((> len-1 len-2) #f)
          ;; Invariant:  equal lengths
          (else
           (let prefix<? ((list-1 list-1) (list-2 list-2))
             (cond ((null? list-1) #f)  ; same
                   ((generic:less? (car list-1) (car list-2)) #t)
                   ((generic:less? (car list-2) (car list-1)) #f)
                   (else (prefix<? (cdr list-1) (cdr list-2)))))))))

(define generic:less? (make-generic-operator 2 "generic:less?"))

(define (boolean<? a b) (and (not a) b))

;(defhandler sequence:null? null? (any-but string? vector? list?))

(define (any-of . vals) (lambda (x) (any (lambda (type?) (eq? type? x)) vals)))

(defhandler generic:less? (constant #f) null? null?)
(defhandler generic:less? boolean<? boolean? boolean?)
(defhandler generic:less? (constant #f) boolean? null?)
(defhandler generic:less? (constant #t) null? boolean?)
(defhandler generic:less? char<? char? char?)
(defhandler generic:less? (constant #f) char? (any-of null? boolean?)
(defhandler generic:less? (constant #t) (any-of null? boolean?) char?)
(defhandler generic:less? < number? number?)
(defhandler generic:less? (constant #f) number? (any-of null? boolean? char?))
(defhandler generic:less? (constant #t) (any-of null? boolean? char?) number?)
(defhandler generic:less? symbol<? symbol? symbol?)
(defhandler generic:less? (constant #f) symbol?
  (any-of null? boolean? char? number?))
(defhandler generic:less? (constant #t)
  (any-of null? boolean? char? number?) symbol?)
(defhandler generic:less? string<? string? string?)
(defhandler generic:less? (constant #f) string?
  (any-of null? boolean? char? number? symbol?))
(defhandler generic:less? (constant #t)
  (any-of null? boolean? char? number? symbol?) string?)
(defhandler generic:less? (lambda (v1 v2) (list<? (vector->list v1) (vector->list v2)))
  vector? vector?)
(defhandler generic:less? (constant #f) vector?
  (any-of null? boolean? char? number? symbol? string?))
(defhandler generic:less? (constant #t)
  (any-of null? boolean? char? number? symbol? string?) vector?)
(defhandler generic:less? list<? list? list?)
(defhandler generic:less? (constant #f) list?
  (any-of null? boolean? char? number? symbol? string? vector?))
(defhandler generic:less? (constant #t)
  (any-of null? boolean? char? number? symbol? string? vector?) list?)
