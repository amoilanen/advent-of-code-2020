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
939
7,13,x,x,59,x,31,19
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

; TODO: Inefficient algorithm: the more the timestamp grows, the more time it requires to compute a reminder
; Devise a more efficient algorithm
(define (find-orderly-departure-timestamp departure-schedule)
  (define (timestamp-match? window)
    (every?
      (lambda (x)
        (equal? x (car window)))
      (cdr window)))
  (let ((match-length (length departure-schedule))
        (normalized-timestamps-stream
          (apply
            stream-merge-ordered
            (cons
              (lambda (x y) (< x y))
              (map
                (lambda (bus-schedule)
                  (stream-map
                    (lambda (x)
                      (- x (car bus-schedule)))
                    (stream-multiples-of (cdr bus-schedule))))
                departure-schedule)))))
    (car
      (stream-find-window
        match-length
        timestamp-match?
        normalized-timestamps-stream))))

(define (find-orderly-departure-timestamp departure-schedule)
  (let ((first-bus-id (cdr (car departure-schedule)))
        (other-bus-schedules (cdr departure-schedule)))
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
    (stream-find
      has-orderly-departure
      (stream-multiples-of first-bus-id))))

; Executing solutions
(newline)
(display "Part 1:")
(newline)
(display
  (answer-to-part-1
    earliest-departure-time
    (find-first-departure
       earliest-departure-time
       (bus-schedule-stream
          bus-ids))))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (find-orderly-departure-timestamp
        departure-schedule))
    write-timings))
(newline)