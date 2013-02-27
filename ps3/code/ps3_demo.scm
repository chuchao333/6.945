;;; Demonstrations for PS3

;; Problem 3.1
((vector sin cos) 0.6)
; #(.5646424733950354 .8253356149096783)
(define (square x) (* x x))
((vector square) 2)
; #(4)
((vector (lambda (x y) (list (sin x) (cos y))) cons) 1 2)
; #((.8414709848078965 -.4161468365471424) (1 . 2))

;; Problem 3.2
(map (lambda (x) (* x x)) '(1 2 3))
; (1 4 9)
(map (lambda (x y) (* x y)) '(1 2 3) '(4 5 6))
; (4 10 18)
(let ((v (make-vector 5)))
  (for-each
   (lambda (i) (vector-set! v i (* i i)))
   '(0 1 2 3 4))
  v)
; #(0 1 4 9 16)

;; Problem 3.3
(+ (* 2 3) (* 4 5))
; 26
(+ (* a 3) (* 4 5))
; (+ (* a 3) 20)
(map (lambda (x) (+ a (* x 4))) '(1 b (* 5 c)))
; ((+ a 4) (+ a (* b 4)) (+ a (* (* 5 c) 4)))
(+ (f 3) (* 4 5))
; (+ (f 3) 20)
(f (+ 3 4))
; (f 7)