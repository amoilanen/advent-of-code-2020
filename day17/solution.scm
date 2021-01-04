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
(define (make-grid-from-layers-vector layers left-top-layer-row-column-indices)
  (define layers-list
    (vector->list
      (vector-map
        (lambda (rows)
          (vector->list
            (vector-map
              vector->list
              rows)))
        layers)))
  (make-grid layers-list left-top-layer-row-column-indices))

(define (make-grid layers-list left-top-layer-row-column-indices)
  (define layers
    (list->vector
      (map
        (lambda (rows-list)
          (list->vector
            (map
              list->vector
              rows-list)))
        layers-list)))
  (define (are-valid-cell-coordinates layer-idx row-idx column-idx)
    (and
      (< layer-idx layer-number)
      (>= layer-idx 0)
      (< row-idx row-number)
      (>= row-idx 0)
      (< column-idx column-number)
      (>= column-idx 0)))
  (define as-list
    layers-list)
  (define (element-at layer-idx row-idx column-idx)
    (if (are-valid-cell-coordinates layer-idx row-idx column-idx)
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
    (display left-top-layer-row-column-indices)
    (newline)
    (map
      (lambda (layer-with-index)
        (let ((initial-layer-idx (car left-top-layer-row-column-indices))
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
          ((eq? op 'left-top) left-top-layer-row-column-indices)
          ((eq? op 'element-at) element-at)
          ((eq? op 'layer-number) layer-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'column-number) column-number)
          (else (error "Unsupported grid op:" op))))
  dispatch)

; Solution
(define (immediately-adjacent-value layer-idx row-idx column-idx layer-dir row-dir column-dir grid)
  (let ((adjacent-layer-idx (+ layer-idx layer-dir))
        (adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir)))
    ((grid 'element-at) adjacent-layer-idx adjacent-row-idx adjacent-column-idx)))

(define adjacent-directions
  (list
    (list -1 -1 -1 )
    (list -1 -1 0 )
    (list -1 -1 1 )
    (list -1 0 -1 )
    (list -1 0 0 )
    (list -1 0 1 )
    (list -1 1 -1 )
    (list -1 1 0 )
    (list -1 1 1 )
    (list 0 -1 -1)
    (list 0 -1 0)
    (list 0 -1 1)
    (list 0 0 -1)
    (list 0 0 1)
    (list 0 1 -1)
    (list 0 1 0)
    (list 0 1 1)
    (list 1 -1 -1)
    (list 1 -1 0)
    (list 1 -1 1)
    (list 1 0 -1)
    (list 1 0 0)
    (list 1 0 1)
    (list 1 1 -1)
    (list 1 1 0)
    (list 1 1 1)))

(define (adjacent-values layer-idx row-idx column-idx grid)
  (filter
    identity
    (map
      (lambda (x)
        (immediately-adjacent-value
          layer-idx
          row-idx
          column-idx
          (car x)
          (cadr x)
          (caddr x)
          grid))
      adjacent-directions)))

(define (updated-value-at layer-idx row-idx column-idx grid)
  (let ((current-value
           ((grid 'element-at) layer-idx row-idx column-idx))
        (adjacent-active-count
           (apply
             +
             (adjacent-values layer-idx row-idx column-idx grid))))
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
  (=
    0
    (active-cubes-number-in-grid-slice
          grid-slice)))

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
    (lambda (left-top)
      (list
        (- (car left-top) 1)
        (cadr left-top)
        (caddr left-top)))
    grid))

(define (remove-empty-row-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 0 -1))
    (lambda (g)
      (list -1 (- (g 'row-number) 1) -1))
    (lambda (left-top)
      (list
        (car left-top)
        (- (cadr left-top) 1)
        (caddr left-top)))
    grid))

(define (remove-empty-column-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 -1 0))
    (lambda (g)
      (list -1 -1 (- (g 'column-number) 1)))
    (lambda (left-top)
      (list
        (car left-top)
        (cadr left-top)
        (- (caddr left-top) 1)))
    grid))

(define (remove-empty-side-slices first-slice-coordinates last-slice-coordinates left-top-update grid)
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
                      (left-top-update left-top))
                    last-slice-handled)))
            (if (or is-first-slice-inactive is-last-slice-inactive)
              (remove-empty-side-slices
                first-slice-coordinates
                last-slice-coordinates
                left-top-update
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
                      layer-index
                      row-index
                      column-index
                      extended-grid))
                  column-indexes))
              row-indexes))
          layer-indexes)
        (extended-grid 'left-top)))))

(define (simulate-cycles initial-grid cycle-count)
  (define (loop grid current-cycle)
    ;(display "Cycle ")
    ;(display current-cycle)
    ;(newline)
    ;(grid 'show-grid)
    ;(newline)
    (if (< current-cycle cycle-count)
      (loop
        (update-grid grid)
        (+ 1 current-cycle))
      grid))
  (loop initial-grid 0))

(define (active-cubes-number-in-grid-slice grid-slice)
  (vector-apply +
    (vector-apply vector-append
      (vector-apply vector-append
        grid-slice))))

(define (active-cubes-number grid)
  (active-cubes-number-in-grid-slice (grid 'layers)))

; Display results
(define initial-grid
  (make-grid
    (list
      (parse-input
        (string->list
          input-data)))
    (list 0 0 0)))

(newline)
(display "Part 1:")
(newline)
(display
  (active-cubes-number
    (simulate-cycles
      initial-grid
      6)))
(newline)