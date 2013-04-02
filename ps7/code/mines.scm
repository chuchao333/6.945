

(define *the-field*)

(define (make-empty-field h w)
  (list->vector (map (lambda (i) (make-empty-row i w)) (iota h))))

(define (make-empty-row i w)
  (list->vector (map (lambda (j) (make-new-cell i j)) (iota w))))

(define (make-new-cell row col)
  (let-cells (the-cell bomb-cell count-cell)
	     (add-content bomb-cell (make-interval 0 1))
	     (eq-put! the-cell 'bomb bomb-cell)
	     (eq-put! the-cell 'row row)
	     (eq-put! the-cell 'col col)
	     (add-content count-cell (make-interval 0 8))
	     (eq-put! the-cell 'bombs-near count-cell)
	     (eq-put! the-cell 'neighbors '())
	     (eq-put! the-cell 'clicked #f)
	     (eq-put! the-cell 'flagged #f)
	     the-cell))

(define (f:inquire cell prop) (eq-get cell prop))
(define (f:bomb cell) (f:inquire cell 'bomb))
(define (f:row cell) (f:inquire cell 'row))
(define (f:col cell) (f:inquire cell 'col))
(define (f:count cell) (f:inquire cell 'bombs-near))
(define (f:neighbors cell) (f:inquire cell 'neighbors))
(define (f:clicked? cell) (f:inquire cell 'clicked))
(define (f:flagged? cell) (f:inquire cell 'flagged))

(define (f:bomb? cell) (= (content (f:bomb cell)) 1))
(define (f:idx cell) (list (f:row cell) (f:col cell)))

(define (safe-vector-ref vec idx)
  (assert (vector? vec) "given vector isn't a vector")
  (let ((l (vector-length vec)))
    (if (< -1 idx l) (vector-ref vec idx) #f)))

(define (field-ref field idx)
  (let ((i (car idx))
	(j (cadr idx)))
    (let ((row (safe-vector-ref field i)))
      (if row (safe-vector-ref row j) row))))

(define (e:++ . cells)
  ;(pp cells)
  (reduce (lambda (c1 c2) (e:+ c1 c2)) (constant 0) cells))

(define (wire-field field)
  (define (wire-cell cell)
    ;; create list of neighbors
    (define (consider-neighbor idx)
      (let ((neighbor (field-ref field idx)))
	(if neighbor ;(begin (pp (list cell neighbor))
		       (list neighbor)
		       ;)
	    '())
	    ))
    (eq-put! cell 'neighbors
	     (reduce (lambda (x y) (append x y)) '()
		     (map consider-neighbor
			  (make-neighbor-indices (f:row cell)
						 (f:col cell)))))

    ;; wire our count to our neighbors
    (let ((neighbors (f:neighbors cell)))
      (define (set-count)
	(p:id (apply e:++ (map f:bomb neighbors))
	      (f:count cell)))

      (cond
       ;; could be a corner cell
       ((= (length neighbors) 3) (set-count))
       ;; could be a borer cell, not on the corner
       ((= (length neighbors) 5) (set-count))
       ;; could be a normal cell
       ((= (length neighbors) 8) (set-count))
       ;; anything else is wrong!
       (else (begin (bkpt 'ah 'ah) (error "impossible number of neighbors" (length neighbors))))
      )
    ))
  ;; iterate over cells in field and wire each
  (for-each (lambda (row)
	      (for-each wire-cell (vector->list row)))
	    (vector->list field)))

(define (make-neighbor-indices i j)
  (let ((i-1 (- i 1)) (i+1 (+ i 1))
	(j-1 (- j 1)) (j+1 (+ j 1)))
    (list
     (list i-1 j-1)
     (list i j-1)
     (list i+1 j-1)
     (list i+1 j)
     (list i+1 j+1)
     (list i j+1)
     (list i-1 j+1)
     (list i-1 j))))



;; Sample n elements from list without replacement
(define (sample n list)
  (cond ((> n (length list)) (error "num samples is larger than list"))
	((= n (length list)) list)
	(else
	 (let lp ((list list)
		  (n n))
	   (if (= n 0) '()
	       (let ((sample (random (length list))))
		 (cons (list-ref list sample) (lp (list-except list sample) (- n 1)))))))))

;; Return list with ith element removed
(define (list-except list i)
  (cond
   ((= i 0) (cdr list))
   ((= i (- (length list) 1)) (list-head list i))
   ((<= 0 i (- (length list) 1)) (append (list-head list i) (list-tail list (+ i 1))))
   (else (error "invalid list index given" i))))

;; assuming alt is subset of list, return all elements of list not in alt
(define (list-xor list alt)
  (let lp ((list list)
	   (alt alt))
    (if (= (length alt) 0) list
	(lp (list-except list
			 (find (lambda (x) (eq? (list-ref list x) (car alt)))
			       (iota (length list))))
	    (cdr alt)))))


(define (set-bombs field bombs)
  (define (set-bomb idx)
    (let ((cell (field-ref field idx)))
      (add-content (f:bomb cell) 1)))
  (define (set-no-bomb idx)
    (let ((cell (field-ref field idx)))
      (add-content (f:bomb cell) 0)))

  (let ((h (vector-length field))
	(w (vector-length (vector-ref field 0))))
    (define (lidx->idx lidx)
      (list (modulo lidx w) (quotient lidx w)))
    (let ((samples (sample bombs (iota (* h w)))))
      (for-each set-bomb (map lidx->idx samples))
      (for-each set-no-bomb (map lidx->idx (list-xor (iota (* h w)) samples))))))

(define (new-game h w bombs)
  (let ((field (make-empty-field h w)))
    (wire-field field)
    (set-bombs field bombs)
    (run)
    field))

(define (disp-field-proc field proc)
  (for-each (lambda (row)
	      (for-each (lambda (cell)
			  (display (proc cell)))
			(vector->list row))
	      (newline))
	    (vector->list field)))

(define (disp-true-field field)
  (disp-field-proc field
		   (lambda (cell)
		     (if (f:bomb? cell) "*"
			 (content (f:count cell))))))

(define (disp-field field)
  (disp-field-proc field
		   (lambda (cell)
		     (cond ((f:clicked? cell)
			    (if (f:bomb? cell) "*"
				(content (f:count cell))))
			   ((f:flagged? cell) "%")
			   (else "?"))))
  (newline))

;; debugging aid
(define (disp-field-prop field prop)
  (disp-field-proc (lambda (cell) (content (prop cell)))))

(define (click field idx)
  (eq-put! (field-ref field idx) 'clicked #t)
  (disp-field field)
  (if (f:bomb? (field-ref field idx)) (begin (display "game over!") (newline))))

(define (flag field idx)
  (eq-put! (field-ref field idx) 'flagged #t)
  (disp-field field))

(define (click-unflagged-neighbors field idx)
  ;(let ((num-flagged (reduce + 0 (map (lambda (cell) (if (f:flagged? cell) 1 0))
  ;				      (f:neighbors (field-ref field idx))))))
  (map (lambda (cell) (click field (f:idx cell))) (f:neighbors (field-ref field idx)))
  (disp-field field))