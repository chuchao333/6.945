;;; Problem 4.1
;; Demonstrate functionality
(define ss (lambda x (map square x)))
(ss 1 2 3 4)
; (1 4 9 16)
(define (ss . x) (map square x))
(ss 1 2 3 4)
; (1 4 9 16)
;; Demonstrate everything still works
(define ss (lambda (x y) (* x y)))
(ss 3 4)
; 12
(define (ss x y) (* x y))
(ss 3 4)
; 12

;;;Problem 4.2
;; Demonstrate functionality
(infix "3+2")
; 5
(infix "3+2-1")
; 4
(infix "3+2+1-1")
; 5
(infix "3-2+1")
; 2
(infix "3+2-1+2-3-4-4")
; -5
(infix "3/2")
; 3/2
(infix "2*3/2")
; 3
(infix "3/2*2")
; 3
(infix "3/2*2/2")
; 3/2
(infix "2^2/2+1")
; 3
(infix "2/2^3*4^0-3^2*3")
; -107/4
(define a 3)
(define b 2)
(infix "a*b")
; 6
(infix "a*a/b^a-a*b/a")
; -7/8

;;; Problem 4.3
;; 4.50
(define count 0)
;; show that count always is 0 with undoable set
(let ((x (amb 'a 'b 'c)) (y (amb 'a 'b 'c))) (set! count (+ count 1)) (require (not (eq? x y))) (list x y count))
; (a b 1)
try-again
; (a c 1)
;; show that count is always incrementing with permanent set
(let ((x (amb 'a 'b 'c)) (y (amb 'a 'b 'c))) (permanent-set! count (+ count 1)) (require (not (eq? x y))) (list x y count))
; (a b 2)
try-again
; (a c 3)

;; 4.51
(if-fail (let ((x (amb '1 3 5)))
	   (require (even? x))
	   x)
	 'all-odd)
; all-odd
(if-fail (let ((x (amb '1 3 5 8)))
	   (require (even? x))
	   x)
	 'all-odd)
; 8
