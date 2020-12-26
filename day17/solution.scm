(load "./lib/list.scm")

(define input-data "
.#.
..#
###
")

; Parser
(define (parse-input input)
  (let ((lines (omit-empty (split-list-by input '#\newline))))
   (map
     parse-line
     lines)))

(define (parse-line input)
  (map
    (lambda (symbol)
      (if (equal? symbol '#\#) 1
        0))
    input))

; Grid
(define (make-grid layers-list center-row-idx-column-idx-layer-idx)
  (define layers
    (list->vector
      (map
        (lambda (rows-list)
          (list->vector
            (map
              list->vector
              rows-list)))
        layers-list)))
  (define (are-valid-cell-coordinates row-idx column-idx layer-idx)
    (and
      (< row-idx row-number)
      (>= row-idx 0)
      (< column-idx column-number)
      (>= column-idx 0)
      (< layer-idx layer-number)
      (>= layer-idx 0)))
  (define as-list
    layers-list)
  ; extends grid by one cell in every direction
  (define (extend)
    (define (extend-layers layers-list)
      (define (new-layer)
        (make-list
          (+ 2 row-number)
          (make-list
            (+ 2 column-number)
            0)))
      (append
        (cons
          (new-layer)
          layers-list)
        (list (new-layer))))
    (define (extend-rows layers-list)
      (define (new-row)
         (make-list (+ 2 column-number) 0))
      (map
        (lambda (layer)
          (append
            (cons
              (new-row)
              layer)
            (list (new-row))))
        layers-list))
    (define (extend-columns layers-list)
      (define new-column 0)
      (map
        (lambda (layer)
          (map
            (lambda (row)
              (append (cons new-column row) (list new-column)))
            layer))
        layers-list))
    (let ((updated-center
            (map
              (lambda (x)
                (+ x 1))
              center-row-idx-column-idx-layer-idx))
          (extended-layers-list
            (extend-layers
              (extend-rows
                (extend-columns
                  layers-list)))))
          (make-grid
            extended-layers-list
            updated-center)))
  (define (element-at row-idx column-idx layer-idx)
    (if (are-valid-cell-coordinates row-idx column-idx layer-idx)
      (let ((layer (vector-ref layers layer-idx)))
        (let ((row (vector-ref layer row-idx)))
          (vector-ref row column-idx)))
      #f))
  (define layer-number
    (vector-length layers))
  (define row-number
    (if (equal? layer-number 0) 0
      (vector-length
        (vector-ref layers 0))))
  (define column-number
    (if (equal? row-number 0) 0
       (vector-length
         (vector-ref
           (vector-ref layers 0)
           0))))
  (define (show-grid)
    (map
      (lambda (layer-with-index)
        (let ((center-layer-idx (caddr center-row-idx-column-idx-layer-idx))
              (layer-idx (car layer-with-index))
              (layer (cadr layer-with-index)))
          (display "z=")
          (display (- layer-idx center-layer-idx))
          (newline)
          (map
            (lambda (row)
              (display row)
              (newline))
            layer)))
      (zip
        (range 0 (- layer-number 1))
        layers-list)))
  (define (dispatch op)
    (cond ((eq? op 'show-grid) (show-grid))
          ((eq? op 'extend) (extend))
          ((eq? op 'layers) layers)
          ((eq? op 'element-at) element-at)
          ((eq? op 'layer-number) layer-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'column-number) column-number)
          (else (error "Unsupported grid op:" op))))
  dispatch)

; Solution
(define (immediately-adjacent-value row-idx column-idx layer-idx row-dir column-dir layer-dir matrix)
  (let ((adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir))
        (adjacent-layer-idx (+ layer-idx layer-dir)))
    ((matrix 'element-at) adjacent-row-idx adjacent-column-idx adjacent-layer-idx)))

(define adjacent-directions
  (list
    (list -1 -1 -1)
    (list -1 0 -1)
    (list -1 1 -1)
    (list 0 -1 -1)
    (list 0 0 -1)
    (list 0 1 -1)
    (list 1 -1 -1)
    (list 1 0 -1)
    (list 1 1 -1)
    (list -1 -1 0)
    (list -1 0 0)
    (list -1 1 0)
    (list 0 -1 0)
    (list 0 1 0)
    (list 1 -1 0)
    (list 1 0 0)
    (list 1 1 0)
    (list -1 -1 1)
    (list -1 0 1)
    (list -1 1 1)
    (list 0 -1 1)
    (list 0 0 1)
    (list 0 1 1)
    (list 1 -1 1)
    (list 1 0 1)
    (list 1 1 1)))

(define (adjacent-values row-idx column-idx layer-idx grid)
  (filter
    identity
    (map
      (lambda (x)
        (immediately-adjacent-value
          row-idx
          column-idx
          layer-idx
          (car x)
          (cadr x)
          (caddr x)
          grid))
      adjacent-directions)))

; TODO: Update the code from task 11 for the task 17
;(define (updated-value-at row-idx column-idx matrix)
;  (let ((current-value
;           ((matrix 'element-at) row-idx column-idx))
;        (adjacent-occupied-count
;           (count-of
;              occupied-seat
;              (adjacent-values row-idx column-idx matrix))))
;    (cond ((and
;              (equal? current-value empty-seat)
;              (equal? 0 adjacent-occupied-count))
;            occupied-seat)
;          ((and
;              (equal? current-value occupied-seat)
;              (>= adjacent-occupied-count seated-tolerance))
;            empty-seat)
;          (else current-value))))
;
;(define (update-matrix seated-tolerance matrix)
;  (define row-indexes (range 0 (- (matrix 'row-number) 1)))
;  (define column-indexes (range 0 (- (matrix 'column-number) 1)))
;  (make-matrix
;    (map
;      (lambda (row-index)
;        (map
;          (lambda (column-index)
;            (updated-value-at
;              row-index
;              column-index
;              seated-tolerance
;              matrix))
;          column-indexes))
;      row-indexes)))

; TODO: Update the grid according to the cube activation rules

; TODO: Filter out after update:
; - empty layers from the results
; - empty side row-wise
; - empty side column-wise

; TODO: Run the update cycle 3 times, verify that the results are as expected
; TODO: Run the update cycle 6 times, display the result
; TODO: Count the number of active cubes after running the cycle 6 times, display the result

; Display results
(define initial-grid
  (make-grid
    (list
      (parse-input
        (string->list
          input-data)))
    (list 0 0 0)))

(newline)
((initial-grid 'extend) 'show-grid)
(newline)