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
  (loop streams (stream-constant-of '())))

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

(newline)
(display "Part 1:")
(newline)
(display
  (parse-input
    (string->list input-data)))
(newline)