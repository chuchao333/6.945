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

;;; Problem 8.5: simple pipe implementation
(define (make-pipe)
  (list (conspire:make-lock) (queue:make)))

(define (lock-pipe pipe)
  (conspire:acquire-lock (car pipe)))

(define (unlock-pipe pipe)
  (conspire:unlock (car pipe)))

(define (write-pipe pipe value)
  (lock-pipe pipe)
  (queue:add-to-end! (cadr pipe) value)
  (unlock-pipe pipe))

(define (pipe-writer pipe)
  (lambda (value) (write-pipe pipe value)))

(define (read-pipe pipe)
  (conspire:switch-threads (lambda () (not (queue:empty? (cadr pipe)))))
  (lock-pipe pipe)
  (let ((value (queue:get-first (cadr pipe))))
    (unlock-pipe pipe)
    value))

(define (pipe-reader pipe)
  (lambda () (read-pipe pipe)))

;;; Problem 8.6
(define (make-threaded-filter generator)
  (let ((pipe (make-pipe)))
    (let ((thread (conspire:make-thread
		   conspire:runnable
		   (lambda ()
		     (generator (pipe-writer pipe))))))
      (pipe-reader pipe))))

(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (f (g (h)))))
   ))
;Value: #t

(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f (h)))))
   ))
;Value: #f

(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f ))))
   ))
;Value: #f