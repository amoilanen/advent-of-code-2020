(load "./lib/identity.scm")
(load "./lib/list.scm")
(load "./lib/vector.scm")

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
(define (make-grid-from-layers-vector layers left-top-row-column-layer-indices)
  (define layers-list
    (vector->list
      (vector-map
        (lambda (rows)
          (vector->list
            (vector-map
              vector->list
              rows)))
        layers)))
  (make-grid layers-list left-top-row-column-layer-indices))

(define (make-grid layers-list left-top-row-column-layer-indices)
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
    (display "left-top=")
    (display left-top-row-column-layer-indices)
    (newline)
    (map
      (lambda (layer-with-index)
        (let ((initial-layer-idx (caddr left-top-row-column-layer-indices))
              (layer-idx (car layer-with-index))
              (layer (cadr layer-with-index)))
          (display "z=")
          (display (- layer-idx initial-layer-idx))
          (newline)
          (map
            (lambda (row)
              (display
                (map
                  (lambda (v)
                    (if (= v 1) '#\# '#\.))
                  row))
              (newline))
            layer)))
      (zip
        (range 0 (- layer-number 1))
        layers-list)))
  (define (dispatch op)
    (cond ((eq? op 'show-grid) (show-grid))
          ((eq? op 'extend) (extend))
          ((eq? op 'layers) layers)
          ((eq? op 'layers-list) layers-list)
          ((eq? op 'left-top) left-top-row-column-layer-indices)
          ((eq? op 'element-at) element-at)
          ((eq? op 'layer-number) layer-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'column-number) column-number)
          (else (error "Unsupported grid op:" op))))
  dispatch)

; Solution
(define (immediately-adjacent-value row-idx column-idx layer-idx row-dir column-dir layer-dir grid)
  (let ((adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir))
        (adjacent-layer-idx (+ layer-idx layer-dir)))
    ((grid 'element-at) adjacent-row-idx adjacent-column-idx adjacent-layer-idx)))

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

(define (updated-value-at row-idx column-idx layer-idx grid)
  (let ((current-value
           ((grid 'element-at) row-idx column-idx layer-idx))
        (adjacent-active-count
           (apply
             +
             (adjacent-values row-idx column-idx layer-idx grid))))
    (cond ((and
              (= current-value 0)
              (= adjacent-active-count 3))
            1)
          ((and
              (= current-value 1)
              (or 
                (< adjacent-active-count 2)
                (> adjacent-active-count 3)))
            0)
          (else current-value))))

; extends grid by one cell in every direction
(define (extend grid)
  (let ((grid-left-top (grid 'left-top))
        (layers-list (grid 'layers-list))
        (layer-number (grid 'layer-number))
        (row-number (grid 'row-number))
        (column-number (grid 'column-number)))
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
    (let ((updated-left-top
            (map
              (lambda (x)
                (+ x 1))
              grid-left-top))
          (extended-layers-list
            (extend-layers
              (extend-rows
                (extend-columns
                  layers-list)))))
          (make-grid
            extended-layers-list
            updated-left-top))))

(define (apply-grid-modification layer-idx row-idx column-idx modification layers)
  (vector-map
    (lambda (layer)
      (vector-map
        (lambda (row)
          (if (= column-idx -1)
            row
            (modification row column-idx)))
        (if (= row-idx -1)
          layer
          (modification layer row-idx))))
    (if (= layer-idx -1)
      layers
      (modification layers layer-idx))))

(define (slice-grid layer-idx row-idx column-idx layers)
  (apply-grid-modification layer-idx row-idx column-idx
    (lambda (grid-part idx)
      (vector (vector-ref grid-part idx)))
    layers))

(define (omit-grid-slice layer-idx row-idx column-idx layers)
  (apply-grid-modification layer-idx row-idx column-idx
    (lambda (grid-part idx)
      (vector-omit-index grid-part idx))
    layers))

(define (is-inactive-grid-slice? grid-slice)
  (vector-every
    (lambda (x)
      (= x 0))
    (vector-apply vector-append
      (vector-apply vector-append
          grid-slice))))

(define (remove-empty-sides grid)
  (remove-empty-layer-sides
    (remove-empty-row-sides
      (remove-empty-column-sides
        grid))))

(define (remove-empty-layer-sides grid)
    (remove-empty-side-slices
    (lambda (g)
      (list 0 -1 -1))
    (lambda (g)
      (list (- (g 'layer-number) 1) -1 -1))
    grid))

(define (remove-empty-row-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 0 -1))
    (lambda (g)
      (list -1 (- (g 'row-number) 1) -1))
    grid))

(define (remove-empty-column-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 -1 0))
    (lambda (g)
      (list -1 -1 (- (g 'column-number) 1)))
    grid))

(define (remove-empty-side-slices first-slice-coordinates last-slice-coordinates grid)
  (let ((layers (grid 'layers))
        (left-top (grid 'left-top)))
    (let ((first-slice
            (apply
              slice-grid
              (append
                (first-slice-coordinates grid)
                (list layers))))
          (last-slice
            (apply
              slice-grid
              (append
                (last-slice-coordinates grid)
                (list layers)))))
      (let ((is-first-slice-inactive (is-inactive-grid-slice? first-slice))
            (is-last-slice-inactive (is-inactive-grid-slice? last-slice)))
        (let ((last-slice-handled
                (if (is-inactive-grid-slice? last-slice)
                  (make-grid-from-layers-vector
                    (apply
                      omit-grid-slice
                      (append
                        (last-slice-coordinates grid)
                        (list layers)))
                    left-top)
                  grid)))
          (let ((first-and-last-slice-handled
                  (if (is-inactive-grid-slice? first-slice)
                    (make-grid-from-layers-vector
                      (apply
                        omit-grid-slice
                        (append
                          (first-slice-coordinates last-slice-handled)
                          (list (last-slice-handled 'layers))))
                      (list
                        (car left-top)
                        (cadr left-top)
                        (- (caddr left-top) 1)))
                    last-slice-handled)))
            (if (or is-first-slice-inactive is-last-slice-inactive)
              (remove-empty-side-slices
                first-slice-coordinates
                last-slice-coordinates
                first-and-last-slice-handled)
              grid)))))))
  
(define (update-grid grid)
  (let ((extended-grid
          (extend grid)))
    (define layer-indexes (range 0 (- (extended-grid 'layer-number) 1)))
    (define row-indexes (range 0 (- (extended-grid 'row-number) 1)))
    (define column-indexes (range 0 (- (extended-grid 'column-number) 1)))
    (remove-empty-sides
      (make-grid
        (map
          (lambda (layer-index)
            (map
              (lambda (row-index)
                (map
                  (lambda (column-index)
                    (updated-value-at
                      row-index
                      column-index
                      layer-index
                      extended-grid))
                  column-indexes))
              row-indexes))
          layer-indexes)
        (extended-grid 'left-top)))))

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

(define updated-grid
  (update-grid
    initial-grid))

(newline)
(updated-grid 'show-grid)
(newline)