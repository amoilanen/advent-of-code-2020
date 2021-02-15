(load "./lib/list.scm")
(load "./lib/identity.scm")
(load "./lib/timings.scm")

(define input-data "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
")

; Parser
(define (parse-input input)
  (let ((tile-inputs
          (omit-empty
            (split-list-by
              (split-list-by
                input
                '#\newline)
                '()))))
    (map
      parse-tile
      tile-inputs)))

(define (parse-tile input)
  (let ((tile-id
         (drop-from-tail
           (cadr
             (split-list-by
               (car input)
               '#\space))
           1))
        (tile-rows (cdr input)))
  (make-tile
    (list->string tile-id)
    tile-rows)))

; Solution
(define (make-tile id rows-list)
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
  (define (show-tile)
    (display "tile-id=")
    (display id)
    (newline)
    (map
      (lambda (row)
        (display row)
        (newline))
      rows-list))
  (define (side-shapes)
    (let ((left-side
            (vector->list
              (vector-map
                (lambda (row)
                  (vector-ref row 0))
                rows)))
          (right-side
            (vector->list
              (vector-map
                (lambda (row)
                  (vector-ref row (- column-number 1)))
                 rows)))
          (top-side
            (vector->list
              (vector-ref rows 0)))
          (bottom-side
            (vector->list
              (vector-ref rows (- row-number 1)))))
        (let ((left-shape (shape-of left-side))
              (left-shape-reversed (shape-of (reverse left-side)))
              (right-shape (shape-of right-side))
              (right-shape-reversed (shape-of (reverse right-side)))
              (top-shape (shape-of top-side))
              (top-shape-reversed (shape-of (reverse top-side)))
              (bottom-shape (shape-of bottom-side))
              (bottom-shape-reversed (shape-of (reverse bottom-side))))
          (list
            (list left-shape right-shape top-shape bottom-shape)
            (list left-shape-reversed right-shape-reversed bottom-shape top-shape)
            (list right-shape left-shape top-shape-reversed bottom-shape-reversed)
            (list right-shape-reversed left-shape-reversed bottom-shape-reversed top-shape-reversed))
        )))
  (define (dispatch op)
    (cond ((eq? op 'id) id)
          ((eq? op 'rows) rows-list)
          ((eq? op 'side-shapes) (side-shapes))
          ((eq? op 'element-at) element-at)
          ((eq? op 'column-number) column-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'show-tile) (show-tile))
          (else (error "Unsupported tile op:" op))))
  dispatch)

(define (shape-of side)
  (define (loop remaining-symbols acc scale)
    (if (null? remaining-symbols) acc
      (let ((next-digit
              (if (equal? '#\# (car remaining-symbols)) 1 0)))
        (loop
          (cdr remaining-symbols)
          (+
            acc
            (* next-digit scale))
          (* 2 scale)))))
  (loop (reverse side) 0 1))

(define (all-side-shapes-of tiles)
  (apply
    append
      (map
        (lambda (tile)
          (apply
            append
            (tile 'side-shapes)))
        tiles)))

(define (side-shape-frequencies-of tiles)
  (map
    ; side shapes for all the tile rotations and turns are ((l r t b) ( l' r' b t) (r l t' b') (r' l' b' t'))
    ; every shape occurs _twice_, this is why we have to divide by 2
    (lambda (shape)
      (cons
        (car shape)
        (/
          (length
            (cdr shape))
          2)))
    (group-by
      (all-side-shapes-of tiles)
      identity)))

(define (tiles-with-two-external-sides tiles shape-frequencies)
  (define (is-external-side? side-shape)
    (= (cdr (assoc side-shape shape-frequencies)) 1))
  (define (number-of-external-sides-in-tile-position side-shapes-in-single-position)
    (apply
      +
      (map
        (lambda (side)
          (if (is-external-side? side) 1 0))
        side-shapes-in-single-position)))
  (define (has-two-external-sides? tile)
    (let ((tile-side-shapes (tile 'side-shapes)))
      (some?
        (lambda (side-shapes)
          (= (number-of-external-sides-in-tile-position side-shapes) 2))
        tile-side-shapes)))
  (filter
    has-two-external-sides?
    tiles))

(define (corner-tiles tiles)
  (tiles-with-two-external-sides
    tiles
    (side-shape-frequencies-of tiles)))

(define (answer-to-part-1 tiles)
  (apply
    *
    (map
      (lambda (tile)
        (string->number
          (tile 'id)))
      (corner-tiles
        tiles))))

; Display results

(define tiles
  (parse-input
    (string->list
      input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (answer-to-part-1 tiles))
(newline)