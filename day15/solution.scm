; To run alocate 500Mb of heap memory:
; scheme --heap 524288 --load day15/solution.scm

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
    (define (to-vector remaining indices)
      (if (null? remaining) indices
        (let ((first (car remaining)))
          (vector-set! indices (car first) (cadr first))
          (to-vector (cdr remaining) indices))))
    (let ((list-of-elements-with-indices 
            (let ((indices (range 1 (length l))))
              (reverse
                (zip
                  (reverse l)
                  indices)))))
      (to-vector list-of-elements-with-indices (make-vector (+ 1 interested-in-nth) ))))
  ; Indices are 1-based rather than 0-based for convenience
  (define (loop current-number current-index indices-of-previous)
    (if (equal? current-index interested-in-nth) current-number
      (let ((last-seen-index
             (vector-ref
               indices-of-previous
               current-number)))
        (let ((next-number
                (if last-seen-index
                  (- current-index last-seen-index)
                  0)))
          (vector-set! indices-of-previous current-number current-index)
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
      (nth-play-number starting-numbers 2020))
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

; Even using a pre-allocated array with Scheme seems to be much slower than using a language with access to more low level
; data structures which are implemented very efficiently, such as JavaScript and arrays, Scheme is ~60 times slower:
; https://gist.github.com/antivanov/16a4a74e5621466f7263796e350ff31b

;Part 1:
;0. 0. .002
;234
;
;Part 2:
;27.98 0. 28.029
;8984