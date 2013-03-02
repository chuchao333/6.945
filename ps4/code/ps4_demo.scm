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