(define input-data "
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
")

(define tree '#\#)
(define step-right 3)
(define step-down 1)

; Utility functions
(define (split-list-by l el)
  (define (split l el splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((eq? (car l) el)
            (split (cdr l) el '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) el (cons (car l) splitted-part) already-splitted))))
  (reverse (split l el '() '())))

(define (nth index list)
  (define (nth-loop n l)
    (cond ((null? l) (error "Index out of bounds" index list))
          ((eq? n 0) (car l))
          (else (nth-loop (- n 1) (cdr l)))))
  (if (< index 0) (error "Index out of bounds" index list)
    (nth-loop index list)))

(define (element-occurences-count el l)
  (length
    (filter
      (lambda (x) (eq? el x))
      l)))

; Parser
(define (parse-matrix input)
  (let ((rows (split-list-by input '#\newline)))
    (make-matrix
      (filter
        (lambda (r) (> (length r) 0))
        rows))))

; Definition of matrix
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
      (error "Indexes"
             (list row-idx column-idx)
             "out of matrix bounds"
             (list row-number column-number))))
  (define row-number
    (length rows))
  (define column-number
    (if (null? rows) 0
       (length (car rows))))
  (define (dispatch op)
    (cond ((eq? op 'as-list) as-list)
          ((eq? op 'element-at) element-at)
          ((eq? op 'column-number) column-number)
          ((eq? op 'row-number) row-number)
          (else (error "Unsupported matrix op:" op))))
  dispatch
)

;Traversal of the matrix
(define (travel matrix step-right step-down)
  (define (descend-and-record-route row-idx column-idx recorded-route)
    (if (<= (matrix 'row-number) row-idx) (reverse recorded-route) ; descended to the bottom
      (let ((current-route-cell ((matrix 'element-at) row-idx column-idx))
            (next-row-idx (+ row-idx step-down))
            (next-column-idx (modulo (+ column-idx step-right) (matrix 'column-number))))
        (descend-and-record-route next-row-idx next-column-idx (cons current-route-cell recorded-route)))))
  (descend-and-record-route 0 0 '()))

(define parsed-matrix
  (parse-matrix
    (string->list input-data)))

(define traveled-route
  (travel parsed-matrix step-right step-down))

(define encountered-trees-number
  (element-occurences-count tree traveled-route))

(newline)
(display traveled-route)

(newline)
(display encountered-trees-number)