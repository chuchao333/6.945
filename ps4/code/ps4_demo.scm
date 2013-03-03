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
;-7/8