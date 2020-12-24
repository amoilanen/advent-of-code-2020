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

; Solution

; Display results

(define initial-grid
  (parse-input
    (string->list
      input-data)))

(newline)
(display
  initial-grid)
(newline)