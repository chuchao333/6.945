;;; Problem 6.1
(define (yacht-club)
  (define maryann 1)
  (define gabrielle 2)
  (define lorna 3)
  (define rosalind 4)
  (define melissa 5)
  (let ((moore_daughter (amb maryann gabrielle lorna rosalind melissa))
	(moore_yacht (amb maryann gabrielle lorna rosalind melissa))
	(hood_daughter (amb maryann gabrielle lorna rosalind melissa))
	(hood_yacht (amb maryann gabrielle lorna rosalind melissa))
	(downing_daughter (amb maryann gabrielle lorna rosalind melissa))
	(downing_yacht (amb maryann gabrielle lorna rosalind melissa))
	(hall_daughter (amb maryann gabrielle lorna rosalind melissa))
	(hall_yacht (amb maryann gabrielle lorna rosalind melissa))
	(parker_daughter (amb maryann gabrielle lorna rosalind melissa))
	(parker_yacht (amb maryann gabrielle lorna rosalind melissa)))
    (define (yacht-owned-by-father-of d)
      (cond ((= moore_daughter d) moore_yacht)
	    ((= hood_daughter d) hood_yacht)
	    ((= downing_daughter d) downing_yacht)
	    ((= hall_daughter d) hall_yacht)
	    ((= parker_daughter d) parker_yacht)
	    (else (amb))))
    ;(require (= moore_daughter maryann))
    (require (= hood_daughter melissa))
    (require (= hood_yacht gabrielle))
    (require (= moore_yacht lorna))
    (require (= hall_yacht rosalind))
    (require (= downing_yacht melissa))
    (require (distinct? (list moore_daughter hood_daughter downing_daughter hall_daughter parker_daughter)))
    (require (distinct? (list moore_yacht hood_yacht downing_yacht hall_yacht parker_yacht)))
    (require (not (= hall_daughter rosalind)))
    (require (not (= parker_daughter gabrielle)))
    (require (= (yacht-owned-by-father-of gabrielle) parker_daughter))
    (pp (list (list 'moore_daughter moore_daughter)
	  (list 'hood_daughter hood_daughter)
	  (list 'downing_daughter downing_daughter)
	  (list 'hall_daughter hall_daughter)
	  (list 'parker_daughter parker_daughter)))
    (amb)))

;(with-depth-first-schedule yacht-club)
;Value: ((moore_daughter 1) (hood_daughter 5) (downing_daughter 3) (hall_daughter 2) (parker_daughter 4))

;;; Problem 6.2A
(define (snark-hunt tree)
  (call-with-current-continuation
   (lambda (t_exit)
     (or (let lp ((tree tree))
	   (cond ((pair? tree) (or (lp (car tree)) (lp (cdr tree))))
	     ((eqv? tree 'snark) (t_exit #t)))
	   #f)
	 #f))))

;(snark-hunt '(((a b c) d (e f)) g (((snark . "oops") h) (i . j))))
; Value: #t
;(snark-hunt '(((a b c) d (e f)) g (((snak . "oops") h) (i . j))))
; Value: #f

;;; Problem 6.2B
(define (snark-hunt/instrumented tree)
  (call-with-current-continuation
   (lambda (t_exit)
     (or (let lp ((tree tree))
	   (pp tree)
	   (cond ((pair? tree) (or (lp (car tree)) (lp (cdr tree))))
	     ((eqv? tree 'snark) (begin (pp 'exit) (t_exit #t))))
	   #f)
	 #f))))

;(snark-hunt/instrumented '(((a b c) d (e f)) g (((snark . "oops") h) (i . j))))
; (((a b c) d (e f)) g (((snark . "oops") h) (i . j)))
; ((a b c) d (e f))
; (a b c)
; a
; (b c)
; b
; (c)
; c
; ()
; (d (e f))
; d
; ((e f))
; (e f)
; e
; (f)
; f
; ()
; ()
; (g (((snark . "oops") h) (i . j)))
; g
; ((((snark . "oops") h) (i . j)))
; (((snark . "oops") h) (i . j))
; ((snark . "oops") h)
; (snark . "oops")
; snark
; exit
; Value: #t

;;; Problem 6.4A
(define (flip-coin)
  (amb #t #f))

(define (flip n)
  (if (= n 0) '()
      (cons (flip-coin) (flip (- n 1)))))

;;; Problem 6.4B
; need to overwrite yield function to use our new extraction method
(define (yield)
  (if (stack&queue-empty? *search-schedule*)
      (*top-level* #f)
      ((get-elm))))

(define (get-elm) (pop! *search-schedule*)) ; keep default as pop!

(define alt #t)

(define (alternate)
  (if alt
      (begin
	(set! alt #f)
	(pop! *search-schedule*))
      (begin
	(set! alt #t)
	(pop-right! *search-schedule*))))

(define (with-left-to-right-alternation thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((get-elm alternate)
		 (alt #t))
       (thunk)))))

(define (with-right-to-left-alternation thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((get-elm alternate)
		 (alt #f))
       (thunk)))))

(define (with-random-order-alternation thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((get-elm (lambda () (pop-random! *search-schedule*))))
       (thunk)))))

;(with-left-to-right-alternation (lambda () (pp (flip 10))))
; (#t #f #t #f #t #f #t #f #t #f)
;(with-right-to-left-alternation (lambda () (pp (flip 10))))
; (#f #t #f #t #f #t #f #t #f #t)
;(with-random-order-alternation (lambda () (pp (flip 10))))
; (#f #t #t #t #t #f #t #t #f #f)
