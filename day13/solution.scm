(load "./lib/list.scm")
(load "./lib/stream.scm")
(load "./lib/timings.scm")

; For part 2 finding the t is equivalent to finding the smallest natural solution for the following system of linear equations 
; and taking t = 7 * x_1
; 
; 7 * x_1 + 1 = 13 * x_2
; 7 * x_1 + 4 = 59 * x_3
; 7 * x_1 + 6 = 31 * x_4
; 7 * x_1 + 7 = 19 * x_5

(define input-data "
1014511
17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,643,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,433,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19
")

; Parser
(define (parse-input input)
  (let ((lines (omit-empty (split-list-by input '#\newline))))
    (list
      (parse-timestamp (car lines))
      (parse-bus-schedules (cadr lines)))))

(define (parse-timestamp input)
  (string->number
    (list->string input)))

(define (parse-bus-schedules input)
  (let ((bus-ids (split-list-by input '#\,)))
    (map
      (lambda (id)
        (list->string id))
      bus-ids)))

(define departure-time-and-bus-ids
  (parse-input
    (string->list input-data)))

(define earliest-departure-time
  (car departure-time-and-bus-ids))

(define bus-ids
  (map
    (lambda (id)
      (string->number id))
    (filter
      (lambda (x)
        (not (equal? x "x")))
      (cadr departure-time-and-bus-ids))))

(define departure-schedule
  (let ((all-schedules (cadr departure-time-and-bus-ids)))
    (let ((schedule-length (length all-schedules)))
      (filter
        (lambda (x) (cdr x))
        (map
          (lambda (x)
            (cons
              (car x)
              (string->number (cadr x))))
          (zip
            (range 0 (- schedule-length 1))
            all-schedules))))))

; Solution
(define (bus-schedule-stream bus-ids)
  (apply stream-merge-ordered
    (cons
      (lambda (x y)
        (< (car x) (car y)))
      (map
        (lambda (bus-id)
          (stream-zip
            (stream-multiples-of bus-id)
            (stream-constant-of bus-id)))
        bus-ids))))

(define (find-first-departure earliest-departure-time bus-schedule-stream)
  (stream-find
    (lambda (bus)
      (>= (car bus) earliest-departure-time))
    bus-schedule-stream))

(define (answer-to-part-1 earliest-departure-time first-departure)
  (*
    (-
      (car first-departure)
      earliest-departure-time)
    (cadr first-departure)))

; Inefficient algorithm: takes too much time to run
(define (find-orderly-departure-timestamp departure-schedule surely-after-timestamp)
  (let ((sorted-departure-schedule
          (sort
            departure-schedule
            (lambda (x y)
              (> (cdr x) (cdr y))))))
    (let ((largest-bus-id-timestamp-offset (car (car sorted-departure-schedule)))
          (largest-bus-id (cdr (car sorted-departure-schedule))))
      (let ((surely-after-timestamp-remainder (remainder surely-after-timestamp largest-bus-id))
            (normalized-sorted-departure-schedule
              (map
               (lambda (x)
                 (cons
                   (- (car x) largest-bus-id-timestamp-offset)
                   (cdr x)))
               sorted-departure-schedule)))
          (let ((last-timestamp-not-to-check (- surely-after-timestamp surely-after-timestamp-remainder))
                (first-bus-id (cdr (car normalized-sorted-departure-schedule)))
                (other-bus-schedules (cdr normalized-sorted-departure-schedule)))
            (define (has-orderly-departure timestamp)
              (every?
                (lambda (bus-schedule)
                  (let ((departure-offset (car bus-schedule))
                        (bus-id (cdr bus-schedule)))
                    (equal?
                      (remainder
                        (+ timestamp departure-offset)
                        bus-id)
                      0)))
                other-bus-schedules))
            (-
              (stream-find
                has-orderly-departure
                (stream-map
                  (lambda (x)
                    (+ last-timestamp-not-to-check x))
                  (stream-multiples-of first-bus-id)))
              largest-bus-id-timestamp-offset))))))

; Executing solutions
(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (answer-to-part-1
        earliest-departure-time
        (find-first-departure
           earliest-departure-time
           (bus-schedule-stream
              bus-ids))))
    write-timings))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (find-orderly-departure-timestamp
        departure-schedule 100000000000000))
    write-timings))
(newline)