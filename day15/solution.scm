(load "./lib/list.scm")
(load "./lib/set.scm")
(load "./lib/stream.scm")
(load "./lib/timings.scm")

(define input-data "0,13,1,16,6,17")

; Parser
(define (parse-starting-numbers  input)
  (let ((numbers (omit-empty (split-list-by input '#\,))))
    (map
      (lambda (n)
        (string->number
          (list->string n)))
      numbers)))

; Solution
(define (nth-play-number previous-numbers interested-in-nth)
  (define (elements-reversed-indexes l)
    (define (to-hashtable remaining table)
      (if (null? remaining) table
        (let ((first (car remaining)))
          (hash-table-set! table (car first) (cadr first))
          (to-hashtable (cdr remaining) table))))
    (let ((list-of-elements-with-indices 
            (let ((indices (range 1 (length l))))
              (reverse
                (zip
                  (reverse l)
                  indices)))))
      (to-hashtable list-of-elements-with-indices (make-strong-eq-hash-table))))
  ; Indices are 1-based rather than 0-based inside nth-play-number
  ; to avoid adding/sibtracting one when matching with interested-in-nth
  (define (loop current-number current-index indices-of-previous)
    (if (equal? current-index interested-in-nth) current-number
      (let ((last-seen-current-number
             (hash-table-ref
               indices-of-previous
               current-number
               (lambda () #f))))
        (let ((next-number
                (if last-seen-current-number
                  (- current-index last-seen-current-number)
                  0)))
          (hash-table-set! indices-of-previous current-number current-index)
          (loop
            next-number
            (+ 1 current-index)
            indices-of-previous)))))
  (loop
    (car previous-numbers)
    (length previous-numbers)
    (elements-reversed-indexes
      (cdr previous-numbers))))

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
      (nth-play-number starting-numbers 2002))
    write-timings))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (nth-play-number starting-numbers 30000000))
    write-timings))
(newline)