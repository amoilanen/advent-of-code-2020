(define input-data "
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
")

; Utility functions
(define (contains? el list)
  (not (eq? false (member el list))))

(define (identity x)
  (values x))

(define (split-list-by-list l splitters)
  (define (split l splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((contains? (car l) splitters)
            (split (cdr l) '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) (cons (car l) splitted-part) already-splitted))))
  (reverse (split l '() '())))

(define (split-list-by l el)
  (split-list-by-list l (list el)))

(define (omit-empty l)
  (filter
    (lambda (p) (> (length p) 0))
    l))

(define (nth index list)
  (define (nth-loop n l)
    (cond ((null? l) (error "Index out of bounds" index list))
          ((eq? n 0) (car l))
          (else (nth-loop (- n 1) (cdr l)))))
  (if (< index 0) (error "Index out of bounds" index list)
    (nth-loop index list)))

(define (range from to)
  (if (> from to) '()
    (cons from (range (+ 1 from) to))))

(define (count-of el l)
  (length
    (filter
      (lambda (x) (equal? x el))
      l)))

; Parser
(define (parse-layout input)
  (make-matrix
    (omit-empty
      (split-list-by input '#\newline))))

; Layout
(define (make-matrix rows)
  (define (are-valid-cell-coordinates row-idx column-idx)
    (and
      (< row-idx row-number)
      (>= row-idx 0)
      (< column-idx column-number)
      (>= column-idx 0)))
  (define as-list
    rows)
  (define (element-at row-idx column-idx)
    (if (are-valid-cell-coordinates row-idx column-idx)
      (let ((row (nth row-idx rows)))
        (nth column-idx row))
      #f))
  (define (adjacent-values row-idx column-idx)
    (let ((adjacent-indexes
             (list
               (cons (- row-idx 1) (- column-idx 1))
               (cons (- row-idx 1) column-idx)
               (cons (- row-idx 1) (+ column-idx 1))
               (cons row-idx (- column-idx 1))
               (cons row-idx (+ column-idx 1))
               (cons (+ row-idx 1) (- column-idx 1))
               (cons (+ row-idx 1) column-idx)
               (cons (+ row-idx 1) (+ column-idx 1)))))
        (filter
          identity
          (map
            (lambda (x)
              (element-at (car x) (cdr x)))
            adjacent-indexes))))
  (define (updated-value-at row-idx column-idx)
    (let ((current-value
             (element-at row-idx column-idx))
          (adjacent-occupied-count
             (count-of
                '#\#
                (adjacent-values row-idx column-idx))))
      (cond ((and
                (equal? current-value '#\L)
                (equal? 0 adjacent-occupied-count))
              '#\#)
            ((and
                (equal? current-value '#\#)
                (>= adjacent-occupied-count 4))
              '#\L)
            (else current-value))))
  (define (updated-matrix)
    (make-matrix
      (map
        (lambda (row-index)
          (map
            (lambda (column-index)
              (updated-value-at row-index column-index))
            (range 0 (- column-number 1))))
        (range 0 (- row-number 1)))))
  (define row-number
    (length rows))
  (define column-number
    (if (null? rows) 0
       (length (car rows))))
  (define (dispatch op)
    (cond ((eq? op 'rows) rows)
          ((eq? op 'adjacent-values) adjacent-values)
          ((eq? op 'updated-value-at) updated-value-at)
          ((eq? op 'updated-matrix) updated-matrix)
          ((eq? op 'element-at) element-at)
          ((eq? op 'column-number) column-number)
          ((eq? op 'row-number) row-number)
          (else (error "Unsupported matrix op:" op))))
  dispatch)


; Solution

(define (update-layout-until-stabilization layout)
  (let ((updated-layout
           ((layout 'updated-matrix))))
    (if (equal? (layout 'rows) (updated-layout 'rows))
      layout
      (update-layout-until-stabilization updated-layout))))

(define (number-of-occupied-seats layout)
  (count-of
    '#\#
    (apply append (layout 'rows))))

; Output

(define layout
  (parse-layout
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (number-of-occupied-seats
    (update-layout-until-stabilization layout)))
(newline)