(load "./lib/identity.scm")
(load "./lib/list.scm")
(load "./lib/vector.scm")
(load "./lib/timings.scm")

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
(define (make-grid-from-layers-vector wlayers left-top-wlayer-layer-row-column-indices)
  (define wlayers-list
    (vector->list
      (vector-map
        (lambda (layers)
          (vector->list
            (vector-map
              (lambda (rows)
                (vector->list
                  (vector-map
                    vector->list
                    rows)))
              layers)))
        wlayers)))
  (make-grid wlayers-list left-top-wlayer-layer-row-column-indices))

(define (make-grid wlayers-list left-top-wlayer-layer-row-column-indices)
  (define wlayers
    (list->vector
      (map
        (lambda (layers-list)
          (list->vector
            (map
              (lambda (rows-list)
                (list->vector
                  (map
                    list->vector
                    rows-list)))
              layers-list)))
        wlayers-list)))
  (define (are-valid-cell-coordinates wlayer-idx layer-idx row-idx column-idx)
    (and
      (< wlayer-idx wlayer-number)
      (>= wlayer-idx 0)
      (< layer-idx layer-number)
      (>= layer-idx 0)
      (< row-idx row-number)
      (>= row-idx 0)
      (< column-idx column-number)
      (>= column-idx 0)))
  (define as-list
    wlayers-list)
  (define (element-at wlayer-idx layer-idx row-idx column-idx)
    (if (are-valid-cell-coordinates wlayer-idx layer-idx row-idx column-idx)
      (let ((wlayer (vector-ref wlayers wlayer-idx)))
        (let ((layer (vector-ref wlayer layer-idx)))
          (let ((row (vector-ref layer row-idx)))
            (vector-ref row column-idx))))
      #f))
  (define wlayer-number
    (vector-length wlayers))
  (define layer-number
    (if (equal? wlayer-number 0) 0
      (vector-length
        (vector-ref wlayers 0))))
  (define row-number
    (if (equal? layer-number 0) 0
       (vector-length
         (vector-ref
           (vector-ref wlayers 0)
           0))))
  (define column-number
    (if (equal? row-number 0) 0
       (vector-length
        (vector-ref
          (vector-ref
            (vector-ref wlayers 0)
            0)
          0))))
  (define (show-grid)
    (display "left-top=")
    (display left-top-wlayer-layer-row-column-indices)
    (newline)
    (map
      (lambda (wlayer-with-index)
        (let ((initial-wlayer-idx (car left-top-wlayer-layer-row-column-indices))
              (wlayer-idx (car wlayer-with-index))
              (wlayer (cadr wlayer-with-index)))
          (map
            (lambda (layer-with-index)
              (let ((initial-layer-idx (cadr left-top-wlayer-layer-row-column-indices))
                    (layer-idx (car layer-with-index))
                    (layer (cadr layer-with-index)))
                (newline)
                (display "z=")
                (display (- layer-idx initial-layer-idx))
                (display ", w=")
                (display (- wlayer-idx initial-wlayer-idx))
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
              wlayer))))
      (zip
        (range 0 (- wlayer-number 1))
        wlayers-list)))
  (define (dispatch op)
    (cond ((eq? op 'show-grid) (show-grid))
          ((eq? op 'extend) (extend))
          ((eq? op 'wlayers) wlayers)
          ((eq? op 'wlayers-list) wlayers-list)
          ((eq? op 'left-top) left-top-wlayer-layer-row-column-indices)
          ((eq? op 'element-at) element-at)
          ((eq? op 'wlayer-number) wlayer-number)
          ((eq? op 'layer-number) layer-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'column-number) column-number)
          (else (error "Unsupported grid op:" op))))
  dispatch)

; Solution
(define (immediately-adjacent-value wlayer-idx layer-idx row-idx column-idx wlayer-dir layer-dir row-dir column-dir grid)
  (let ((adjacent-wlayer-idx (+ wlayer-idx wlayer-dir))
        (adjacent-layer-idx (+ layer-idx layer-dir))
        (adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir)))
    ((grid 'element-at) adjacent-wlayer-idx adjacent-layer-idx adjacent-row-idx adjacent-column-idx)))

(define (compute-adjacent-directions dimensions-number)
  (let ((possible-directions
          (list -1 0 1))
        (combination-of-zeros
          (make-list dimensions-number 0)))
    (define (loop remaining-dimensions acc)
      (if (= remaining-dimensions 0) acc
        (let ((updated-acc
                (apply
                  append
                  (map
                    (lambda (direction)
                      (map
                        (lambda (combination)
                           (cons direction combination))
                        acc))
                    possible-directions))))
          (loop
            (- remaining-dimensions 1)
            updated-acc))))
    (filter
      (lambda (combination)
        (not
          (equal?
            combination-of-zeros
            combination)))
      (loop dimensions-number (list '())))))

(define adjacent-directions
  (compute-adjacent-directions 4))

(define (adjacent-values wlayer-idx layer-idx row-idx column-idx grid)
  (filter
    identity
    (map
      (lambda (x)
        (immediately-adjacent-value
          wlayer-idx
          layer-idx
          row-idx
          column-idx
          (car x)
          (cadr x)
          (caddr x)
          (cadddr x)
          grid))
      adjacent-directions)))

(define (updated-value-at wlayer-idx layer-idx row-idx column-idx grid)
  (let ((current-value
          ((grid 'element-at) wlayer-idx layer-idx row-idx column-idx))
        (adjacent-active-count
           (apply
             +
             (adjacent-values wlayer-idx layer-idx row-idx column-idx grid))))
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
(define (extend grid dimensions-number)
  (let ((grid-left-top (grid 'left-top))
        (wlayers-list (grid 'wlayers-list))
        (wlayer-number (grid 'wlayer-number))
        (layer-number (grid 'layer-number))
        (row-number (grid 'row-number))
        (column-number (grid 'column-number)))
    (define (extend-wlayers wlayers-list)
      (define (new-wlayer)
        (make-list
          (+ 2 layer-number)
          (make-list
            (+ 2 row-number)
            (make-list
              (+ 2 column-number)
              0))))
      (append
        (cons
          (new-wlayer)
          wlayers-list)
        (list (new-wlayer))))
    (define (extend-layers wlayers-list)
      (define (new-layer)
        (make-list
          (+ 2 row-number)
          (make-list
            (+ 2 column-number)
            0)))
      (map
        (lambda (wlayer)
          (append
            (cons
              (new-layer)
              wlayer)
            (list (new-layer))))
        wlayers-list))
    (define (extend-rows wlayers-list)
      (define (new-row)
        (make-list (+ 2 column-number) 0))
        (map
          (lambda (wlayer)
            (map
              (lambda (layer)
                (append
                  (cons
                    (new-row)
                    layer)
                  (list (new-row))))
              wlayer))
          wlayers-list))
    (define (extend-columns wlayers-list)
      (define new-column 0)
        (map
          (lambda (wlayer)
            (map
              (lambda (layer)
                (map
                  (lambda (row)
                    (append (cons new-column row) (list new-column)))
                  layer))
              wlayer))
          wlayers-list))
    (let ((updated-left-top
            (if (> dimensions-number 3)
              (map
                (lambda (x)
                   (+ x 1))
                grid-left-top)
              (cons
                (car grid-left-top)
                (map
                  (lambda (x)
                     (+ x 1))
                  (cdr grid-left-top)))))
          (extended-inner-dimensions
            (extend-layers
              (extend-rows
                (extend-columns
                  wlayers-list)))))
          (let ((updated-wlayers
                  (if (> dimensions-number 3)
                    (extend-wlayers extended-inner-dimensions)
                    extended-inner-dimensions)))
            (make-grid
              updated-wlayers
              updated-left-top)))))

(define (apply-grid-modification wlayer-idx layer-idx row-idx column-idx modification wlayers)
  (vector-map
    (lambda (wlayer)
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
          wlayer
          (modification wlayer layer-idx))))
    (if (= wlayer-idx -1)
      wlayers
      (modification wlayers wlayer-idx))))

(define (slice-grid wlayer-idx layer-idx row-idx column-idx wlayers)
  (apply-grid-modification wlayer-idx layer-idx row-idx column-idx
    (lambda (grid-part idx)
      (vector (vector-ref grid-part idx)))
    wlayers))

(define (omit-grid-slice wlayer-idx layer-idx row-idx column-idx wlayers)
  (apply-grid-modification wlayer-idx layer-idx row-idx column-idx
    (lambda (grid-part idx)
      (vector-omit-index grid-part idx))
    wlayers))

(define (is-inactive-grid-slice? grid-slice)
  (=
    0
    (active-cubes-number-in-grid-slice
          grid-slice)))

(define (remove-empty-sides grid)
  (remove-empty-wlayer-sides
    (remove-empty-layer-sides
      (remove-empty-row-sides
        (remove-empty-column-sides
          grid)))))

(define (remove-empty-wlayer-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list 0 -1 -1 -1))
    (lambda (g)
      (list (- (g 'wlayer-number) 1) -1 -1 -1))
    (lambda (left-top)
      (list
        (- (car left-top) 1)
        (cadr left-top)
        (caddr left-top)
        (caddr left-top)))
    grid))

(define (remove-empty-layer-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 0 -1 -1))
    (lambda (g)
      (list -1 (- (g 'layer-number) 1) -1 -1))
    (lambda (left-top)
      (list
        (car left-top)
        (- (cadr left-top) 1)
        (caddr left-top)
        (cadddr left-top)))
    grid))

(define (remove-empty-row-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 -1 0 -1))
    (lambda (g)
      (list -1 -1 (- (g 'row-number) 1) -1))
    (lambda (left-top)
      (list
        (car left-top)
        (cadr left-top)
        (- (caddr left-top) 1)
        (cadddr left-top)))
    grid))

(define (remove-empty-column-sides grid)
  (remove-empty-side-slices
    (lambda (g)
      (list -1 -1 -1 0))
    (lambda (g)
      (list -1 -1 -1 (- (g 'column-number) 1)))
    (lambda (left-top)
      (list
        (car left-top)
        (cadr left-top)
        (caddr left-top)
        (- (cadddr left-top) 1)))
    grid))

(define (remove-empty-side-slices first-slice-coordinates last-slice-coordinates left-top-update grid)
  (let ((wlayers (grid 'wlayers))
        (left-top (grid 'left-top)))
    (let ((first-slice
            (apply
              slice-grid
              (append
                (first-slice-coordinates grid)
                (list wlayers))))
          (last-slice
            (apply
              slice-grid
              (append
                (last-slice-coordinates grid)
                (list wlayers)))))
      (let ((is-first-slice-inactive (is-inactive-grid-slice? first-slice))
            (is-last-slice-inactive (is-inactive-grid-slice? last-slice)))
        (let ((last-slice-handled
                (if (is-inactive-grid-slice? last-slice)
                  (make-grid-from-layers-vector
                    (apply
                      omit-grid-slice
                      (append
                        (last-slice-coordinates grid)
                        (list wlayers)))
                    left-top)
                  grid)))
          (let ((first-and-last-slice-handled
                  (if (is-inactive-grid-slice? first-slice)
                    (make-grid-from-layers-vector
                      (apply
                        omit-grid-slice
                        (append
                          (first-slice-coordinates last-slice-handled)
                          (list (last-slice-handled 'wlayers))))
                      (left-top-update left-top))
                    last-slice-handled)))
            (if (or is-first-slice-inactive is-last-slice-inactive)
              (remove-empty-side-slices
                first-slice-coordinates
                last-slice-coordinates
                left-top-update
                first-and-last-slice-handled)
              grid)))))))

(define (update-grid grid dimensions-number)
  (let ((extended-grid
          (extend grid dimensions-number)))
    (define wlayer-indexes
      (range 0 (- (extended-grid 'wlayer-number) 1)))
    (define layer-indexes
      (range 0 (- (extended-grid 'layer-number) 1)))
    (define row-indexes
      (range 0 (- (extended-grid 'row-number) 1)))
    (define column-indexes
      (range 0 (- (extended-grid 'column-number) 1)))
    (remove-empty-sides
      (make-grid
        (map
          (lambda (wlayer-index)
            (map
              (lambda (layer-index)
                (map
                  (lambda (row-index)
                    (map
                      (lambda (column-index)
                        (updated-value-at
                          wlayer-index
                          layer-index
                          row-index
                          column-index
                          extended-grid))
                      column-indexes))
                  row-indexes))
              layer-indexes))
          wlayer-indexes)
        (extended-grid 'left-top)))))

(define (simulate-cycles initial-grid cycle-count dimensions-number)
  (define (loop grid current-cycle)
    ;(display "Cycle ")
    ;(display current-cycle)
    ;(newline)
    ;(grid 'show-grid)
    ;(newline)
    (if (< current-cycle cycle-count)
      (loop
        (update-grid grid dimensions-number)
        (+ 1 current-cycle))
      grid))
  (loop initial-grid 0))

(define (active-cubes-number-in-grid-slice grid-slice)
  (vector-apply +
    (vector-apply vector-append
      (vector-apply vector-append
        (vector-apply vector-append
          grid-slice)))))

(define (active-cubes-number grid)
  (active-cubes-number-in-grid-slice (grid 'wlayers)))

; Display results
(define initial-grid
  (make-grid
    (list
      (list
        (parse-input
          (string->list
            input-data))))
    (list 0 0 0 0)))

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (active-cubes-number
        (simulate-cycles
          initial-grid
          6
          3)))
    write-timings))
(newline)


(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (active-cubes-number
        (simulate-cycles
          initial-grid
          6
          4)))
    write-timings))
(newline)