

(define *the-field*)

(define (make-empty-field h w)
  (list->vector (map (lambda (i) (make-empty-row i w)) (iota h))))

(define (make-empty-row i w)
  (list->vector (map (lambda (j) (make-new-cell i j)) (iota w))))

(define (make-new-cell row col)
  (let-cells (the-cell)
	     (eq-put! the-cell 'bomb (make-interval 0 1))
	     (eq-put! the-cell 'row row)
	     (eq-put! the-cell 'col col)
	     (eq-put! the-cell 'bombs-near (make-interval 0 8))
	     (eq-put! the-cell 'neighbors '())
	     the-cell))

(define (f:inquire cell prop) (eq-get cell prop))
(define (f:bomb cell) (f:inquire cell 'bomb))
(define (f:row cell) (f:inquire cell 'row))
(define (f:col cell) (f:inquire cell 'col))
(define (f:count cell) (f:inquire cell 'bombs-near))
(define (f:neighbors cell) (f:inquire cell 'neighbors))

(define (safe-vector-ref vec idx)
  (let ((l (vector-length vec)))
    (if (< -1 idx l) (vector-ref vec idx) #f)))

(define (field-ref field i j)
  (safe-vector-ref (safe-vector-ref field j) i))

(define (wire-field field)
  (define (wire-cell cell)
    ;; create list of neighbors
    ;; TODO: this shouldn't need to use side-effects!
    (define (consider-neighbor i j)
      (let ((neighbor (field-ref field i j)))
	(if neighbor (append! (f:neighbors cell) neighbor))))
    (for-each (lambda (neighbor-idx)
		(apply consider-neighbor neighbor-idx))
	      (make-neighbor-indices (f:row cell)
				     (f:col cell)))
    ;; wire our count to our neighbors
    (let ((neighbors (f:neighbors cell)))
      (define (set-count)
	(p:== (f:count cell) (reduce (lambda (c1 c2)
				       (e:+ c1 (f:bomb c2)))
				     (constant 0)
				     neighbors)))
      (cond
       ;; could be a corner cell
       ((= (length neighbors) 3) (set-count))
       ;; could be a borer cell, not on the corner
       ((= (length neighbors) 5) (set-count))
       ;; could be a normal cell
       ((= (length neighbors) 8) (set-count))
       ;; anything else is wrong!
       (else (error "impossible number of neighbors" (length neighbors)))
      )
    ))
  ;; iterate over cells in field and wire each
  (for-each (lambda (row)
	      (for-each (lambda (cell) (wire-cell cell)) row)
	      field)))

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

;(define (set-bombs field bombs))

(define (new-game h w bombs)
  (let ((field (make-empty-field h w)))
    (wire-field field)
    (set-bombs field bombs)
    ))
