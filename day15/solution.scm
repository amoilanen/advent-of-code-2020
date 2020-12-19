(load "./lib/list.scm")
(load "./lib/set.scm")
(load "./lib/stream.scm")
(load "./lib/timings.scm")

(define input-data "0,13,1,16,6,17")
(define interested-in-index 2020)

; Parser
(define (parse-starting-numbers  input)
  (let ((numbers (omit-empty (split-list-by input '#\,))))
    (map
      (lambda (n)
        (string->number
          (list->string n)))
      numbers)))

; Solution
(define (compute-next-number previous-numbers)
  (let ((occured-before-index
          (first-index-of
            (car previous-numbers)
            (cdr previous-numbers))))
    (if occured-before-index
              (+ 1 occured-before-index)
              0)))

(define (stream-of-numbers previous-numbers)
  (let ((next-number (compute-next-number previous-numbers)))
    (cons
      next-number
      (lambda ()
        (stream-of-numbers
          (cons next-number previous-numbers))))))

(define (nth-play-number previous-numbers interested-in-nth)
  (let ((mth-in-stream
          (-
            interested-in-nth
            (length previous-numbers))))
     (car
       (reverse
         (stream-take
           (stream-of-numbers
             previous-numbers)
           mth-in-stream)))))

; Display the results
(define starting-numbers
  (reverse
    (parse-starting-numbers
      (string->list input-data))))


(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (nth-play-number starting-numbers interested-in-index))
    write-timings))
(newline)