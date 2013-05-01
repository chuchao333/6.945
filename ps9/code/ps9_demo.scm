;;; Problem 8.2b
(load "load")
(init)
(define x 3)

(define foo (alpha (a) (if (= x 2) (display 'ha) (display 'oh))))
(pp x)
;; 3
(stop-time-sharing)
(foo (set! x 2))
;; actor-applied
(pp x)
;; 2  <== argument was evaluated, even though actor wasn't run
(start-time-sharing)
;; ha
(load "ps9_code")
(set! x 3)
(pp x)
;; 3
(stop-time-sharing)
(foo (set! x 2))
;; actor-applied
(pp x)
;; 3
(start-time-sharing)
;; ha
(pp x)
;; 2