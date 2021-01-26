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

; Layout
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
  (define (dispatch op)
    (cond ((eq? op 'id) id)
          ((eq? op 'rows) rows-list)
          ((eq? op 'element-at) element-at)
          ((eq? op 'column-number) column-number)
          ((eq? op 'row-number) row-number)
          ((eq? op 'show-tile) (show-tile))
          (else (error "Unsupported tile op:" op))))
  dispatch)

(newline)
(map
  (lambda (tile)
    (tile 'show-tile))
  (parse-input
    (string->list
      input-data)))
(newline)