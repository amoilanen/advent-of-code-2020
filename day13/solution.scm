(load "./lib/list.scm")
(load "./lib/stream.scm")
(load "./lib/timings.scm")

; For part 2 we need to solve a system of modulo equations:
; 
; x = (mod 7) 0
; x = (mod 13) -1
; x = (mod 59) -4
; x = (mod 31) -6
; x = (mod 19) -7
;
; where the bases of the modulo are prime numbers

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

(define (find-orderly-departure-timestamp departure-schedule)
  (define (try-next-timestamp timestamp step remaining-bus-schedules)
    (if (null? remaining-bus-schedules) timestamp
      (let ((next-bus-schedule (car remaining-bus-schedules)))
        (let ((departure-offset (car next-bus-schedule))
              (bus-id (cdr next-bus-schedule)))
          (if (equal?
                (remainder
                  (+ timestamp departure-offset)
                  bus-id)
                0)
            (try-next-timestamp
              timestamp
              (* step bus-id)
              (cdr remaining-bus-schedules))
            (try-next-timestamp
              (+ timestamp step)
              step
              remaining-bus-schedules))))))
  (let ((sorted-departure-schedule
          (sort
            departure-schedule
            (lambda (x y)
              (> (cdr x) (cdr y))))))
    (try-next-timestamp 1 1 sorted-departure-schedule)))

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