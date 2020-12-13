(define input-data "
939
7,13,x,x,59,x,31,19
")


; Utility functions
(define (contains? el list)
  (not (eq? false (member el list))))

(define (split-list-by-list l splitters)
  (define (split l splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((contains? (car l) splitters)
            (split (cdr l) '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) (cons (car l) splitted-part) already-splitted))))
  (reverse (split l '() '())))

(define (split-list-by l el)
  (split-list-by-list l (list el)))

(define (omit-empty l)
  (filter
    (lambda (p) (> (length p) 0))
    l))

; Stream implementation
(define (force x)
  (x))

(define (stream-numbers-from n)
  (cons
    n
    (lambda () (stream-numbers-from (+ n 1)))))

(define (stream-multiples-of n)
  (stream-map
    (lambda (x) (* n x))
    (stream-numbers-from 1)))

(define (stream-constant-of x)
  (cons
    x
    (lambda () (stream-constant-of x))))

(define (stream-take-until predicate stream)
  (define (loop predicate accumulated rest)
    (let ((next-value (car rest)))
      (if (predicate next-value)
        (reverse (cons next-value accumulated))
        (loop predicate (cons next-value accumulated) (force (cdr rest))))))
  (loop predicate '() stream))

(define (stream-find predicate stream)
  (define (loop predicate rest)
    (let ((next-value (car rest)))
      (if (predicate next-value)
        next-value
        (loop predicate (force (cdr rest))))))
  (loop predicate stream))

(define (stream-take stream n)
  (define (loop rest remaining-n accumulated)
    (if (<= remaining-n 0) (reverse accumulated)
      (let ((next-value (car rest)))
        (loop (force (cdr rest)) (- remaining-n 1) (cons next-value accumulated)))))
  (loop stream n '()))

(define (stream-drop stream n)
  (define (loop rest remaining-n)
    (if (<= remaining-n 0) rest
        (loop (force (cdr rest)) (- remaining-n 1))))
  (loop stream n))

(define (stream-map f stream)
  (cons
    (f (car stream))
    (lambda ()
      (stream-map
        f
        (force (cdr stream))))))

(define (stream-zip . streams)
  (define (stream-zip-two s1 s2)
    (let ((first (car s1))
          (second (car s2)))
      (cons
        (cons first second)
        (lambda ()
          (stream-zip-two
            (force (cdr s1))
            (force (cdr s2)))))))
  (define (loop rest-of-streams result)
    (if (null? rest-of-streams) result
      (let ((first-stream (car rest-of-streams)))
        (loop (cdr rest-of-streams) (stream-zip-two first-stream result)))))
  (loop (reverse streams) (stream-constant-of '())))

(define (stream-merge-ordered ordering . streams)
  (define (stream-merge-two-ordered ordering s1 s2)
    (let ((first (car s1))
          (second (car s2)))
      (if (ordering first second)
        (cons
          first
          (lambda ()
            (stream-merge-two-ordered
              ordering
              (force (cdr s1))
              s2)))
        (cons
          second
          (lambda ()
            (stream-merge-two-ordered
               ordering
               s1
               (force (cdr s2))))))))
  (define (loop rest-of-streams result)
    (if (null? rest-of-streams) result
      (let ((first-stream (car rest-of-streams)))
        (loop (cdr rest-of-streams) (stream-merge-two-ordered ordering first-stream result)))))
  (loop (cdr streams) (car streams)))

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
        (string->number id))
      (filter
        (lambda (id)
          (not (equal? id "x")))
        (map
          (lambda (id)
            (list->string id))
          bus-ids)))))

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

; Display

(define departure-time-and-bus-ids
  (parse-input
    (string->list input-data)))

(define earliest-departure-time
  (car departure-time-and-bus-ids))

(define bus-ids
  (cadr departure-time-and-bus-ids))

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