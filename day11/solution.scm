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

(define occupied-seat '#\#)
(define empty-seat '#\L)

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
(define (make-matrix rows-list)
  (define rows
    (list->vector
      (map
        list->vector
        rows-list)))
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
      (let ((row (vector-ref rows row-idx)))
        (vector-ref row column-idx))
      #f))
  (define row-number
    (vector-length rows))
  (define column-number
    (if (null? rows) 0
       (vector-length (vector-ref rows 0))))
  (define (dispatch op)
    (cond ((eq? op 'rows) rows-list)
          ((eq? op 'element-at) element-at)
          ((eq? op 'column-number) column-number)
          ((eq? op 'row-number) row-number)
          (else (error "Unsupported matrix op:" op))))
  dispatch)

; Solution
(define (immediately-adjacent-value row-idx column-idx row-dir column-dir matrix)
  (let ((adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir)))
    ((matrix 'element-at) adjacent-row-idx adjacent-column-idx)))

(define (directionally-adjacent-value row-idx column-idx row-dir column-dir matrix)
  (let ((adjacent-row-idx (+ row-idx row-dir))
        (adjacent-column-idx (+ column-idx column-dir)))
    (let ((adjacent-value
             ((matrix 'element-at) adjacent-row-idx adjacent-column-idx)))
      (cond ((equal? adjacent-value #f) #f)
            ((or
                (equal? adjacent-value occupied-seat)
                (equal? adjacent-value empty-seat))
              adjacent-value)
            (else (directionally-adjacent-value
                     adjacent-row-idx
                     adjacent-column-idx
                     row-dir
                    column-dir
                     matrix))))))

(define adjacent-directions
  (list
    (cons -1 -1)
    (cons -1 0)
    (cons -1 1)
    (cons 0 -1)
    (cons 0 1)
    (cons 1 -1)
    (cons 1 0)
    (cons 1 1)))

(define (adjacent-values row-idx column-idx adjacent-value-selector matrix)
  (filter
    identity
    (map
      (lambda (x)
        (adjacent-value-selector
          row-idx
          column-idx
          (car x)
          (cdr x)
          matrix))
      adjacent-directions)))

(define (updated-value-at row-idx column-idx seated-tolerance adjacent-value-selector matrix)
  (let ((current-value
           ((matrix 'element-at) row-idx column-idx))
        (adjacent-occupied-count
           (count-of
              occupied-seat
              (adjacent-values row-idx column-idx adjacent-value-selector matrix))))
    (cond ((and
              (equal? current-value empty-seat)
              (equal? 0 adjacent-occupied-count))
            occupied-seat)
          ((and
              (equal? current-value occupied-seat)
              (>= adjacent-occupied-count seated-tolerance))
            empty-seat)
          (else current-value))))

(define (update-matrix seated-tolerance adjacent-value-selector matrix)
  (define row-indexes (range 0 (- (matrix 'row-number) 1)))
  (define column-indexes (range 0 (- (matrix 'column-number) 1)))
  (make-matrix
    (map
      (lambda (row-index)
        (map
          (lambda (column-index)
            (updated-value-at
              row-index
              column-index
              seated-tolerance
              adjacent-value-selector
              matrix))
          column-indexes))
      row-indexes)))

(define (update-layout-until-stabilization seated-tolerance adjacent-value-selector layout)
  (let ((updated-layout
           (update-matrix seated-tolerance adjacent-value-selector layout)))
    (if (equal? (layout 'rows) (updated-layout 'rows))
      layout
      (update-layout-until-stabilization seated-tolerance adjacent-value-selector updated-layout))))

(define (number-of-occupied-seats layout)
  (count-of
    occupied-seat
    (apply append (layout 'rows))))

; Output

(define layout
  (parse-layout
    (string->list input-data)))

(define (write-timings run-time gc-time real-time)
  (write (internal-time/ticks->seconds run-time))
  (write-char #\space)
  (write (internal-time/ticks->seconds gc-time))
  (write-char #\space)
  (write (internal-time/ticks->seconds real-time))
  (newline))

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (number-of-occupied-seats
        (update-layout-until-stabilization
          4
          immediately-adjacent-value
          layout)))
    write-timings))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (number-of-occupied-seats
        (update-layout-until-stabilization
          5
          directionally-adjacent-value
          layout)))
    write-timings))
(newline)