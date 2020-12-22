(load "./lib/list.scm")
(load "./lib/vector.scm")
(load "./lib/parser.scm")
(load "./lib/timings.scm")

(define input-data "
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
")

; Parser
(define (parse-input input)
  (let ((lines (cdr (split-list-by input '#\newline))))
    (let ((input-sections (split-list-by lines '())))
      (make-ticket-notes
        (parse-ticket-rules
          (car input-sections))
        (parse-your-ticket
          (cadr input-sections))
        (parse-nearby-tickets
          (caddr input-sections))))))

(define (parse-ticket-rules input)
  (map
    parse-ticket-rule
    input))

(define (parse-ticket-rule input)
  (define (parse-range input)
    (let ((parts (split-list-by input '#\-)))
      (let ((from
              (string->number
                (list->string (car parts))))
            (to
              (string->number
                (list->string (cadr parts)))))
        (make-range from to))))
  (define (parse-ranges input)
    (let ((range-inputs
            (omit-empty
              (split-list-by-list
                input
                (string->list "or")))))
      (map
        parse-range
        range-inputs)))
  (let ((parts
          (split-list-by
            (strip-spaces input)
            '#\:)))
    (let ((name
            (list->string (car parts)))
          (ranges
            (parse-ranges (cadr parts))))
      (make-rule name ranges))))

(define (parse-your-ticket input)
  (parse-ticket (cadr input)))

(define (parse-nearby-tickets input)
  (map
    parse-ticket
    (cdr input)))

(define (parse-ticket input)
  (let ((numbers (split-list-by input '#\,)))
    (map
      (lambda (x)
        (string->number
          (list->string x)))
      numbers)))

; Models
(define (make-ticket-notes ticket-rules your-ticket nearby-tickets)
  (define (dispatch op)
    (cond ((eq? op 'ticket-rules) ticket-rules)
          ((eq? op 'your-ticket) your-ticket)
          ((eq? op 'nearby-tickets) nearby-tickets)
          ((eq? op 'as-list) (list ticket-rules your-ticket nearby-tickets))
          (else (error "Unsupported op for ticket-notes:" op))))
  dispatch)

(define (make-rule field-name ranges)
  (define (is-valid? value)
    (some?
      (lambda (range)
        (and
          (>= value (car range))
          (<= value (cdr range))))
      ranges))
  (define (dispatch op)
    (cond ((eq? op 'field-name) field-name)
          ((eq? op 'ranges) ranges)
          ((eq? op 'is-valid?) is-valid?)
          ((eq? op 'as-list) (list field-name ranges))
          (else (error "Unsupported op for rule:" op))))
  dispatch)

(define (make-range from to)
  (cons from to))

(define (make-ticket field-values)
  field-values)

; Solution

(define (is-valid-field-value? value rules)
  (some?
    (lambda (rule)
      ((rule 'is-valid?) value))
    rules))

(define (select-invalid-fields ticket-notes)
  (let ((rules (ticket-notes 'ticket-rules))
        (all-field-values
          (apply
            append
            (ticket-notes 'nearby-tickets))))
    (filter
      (lambda (field-value)
        (not
          (is-valid-field-value?
            field-value
            rules)))
      all-field-values)))

(define (solution-for-part-1 ticket-notes)
  (apply
    +
    (select-invalid-fields ticket-notes)))

(define (is-valid-ticket? rules)
  (lambda (ticket)
    (every?
      (lambda (field-value)
        (is-valid-field-value? field-value rules))
      ticket)))

(define (only-valid-tickets tickets rules)
  (filter
    (is-valid-ticket? rules)
    tickets))

(define (ticket-field-values-to-columns tickets)
  (define (update-column-values ticket-values current-index updated-columns)
    (if (null? ticket-values) updated-columns
      (update-column-values
        (cdr ticket-values)
        (+ 1 current-index)
        (vector-set
          updated-columns
          current-index
          (cons
            (car ticket-values)
            (vector-ref
              updated-columns
              current-index))))))
  (define (ticket-loop remaining-tickets columns)
    (if (null? remaining-tickets) columns
      (ticket-loop
        (cdr remaining-tickets)
        (update-column-values
          (car remaining-tickets)
          0
          columns))))
  (let ((value-columns
         (make-vector
           (length
             (car tickets))
           '()))) 
    (vector->list (ticket-loop tickets value-columns))))

(define (find-possible-field-column-indexes field-value-columns rules)
  (define (find-possible-indices rule)
    (find-indexes-where
      (lambda (column)
        (every?
          (lambda (field-value)
            ((rule 'is-valid?) field-value))
          column))
      field-value-columns))
  (define (loop remaining-rules fields-to-indexes)
    (if (null? remaining-rules) fields-to-indexes
      (let ((next-rule (car remaining-rules)))
        (let ((found-index
                (find-possible-indices next-rule)))
          (loop
            (cdr remaining-rules)
            (cons
              (cons
                (next-rule 'field-name)
                found-index)
              fields-to-indexes))))))
  (loop rules '()))

; TODO: Determine the combination of the indexes for the rules that covers all the columns
; TODO: Display the answer from the fields of the current ticket

; Display the answers

(define ticket-notes
  (parse-input
    (string->list input-data)))

(define ticket-rules
  (ticket-notes 'ticket-rules))

(define your-ticket
  (ticket-notes 'your-ticket))

(define nearby-tickets
  (ticket-notes 'nearby-tickets))

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (solution-for-part-1 ticket-notes))
    write-timings))
(newline)

; ((row . 0) (class . 1) (seat . 2))
(newline)
(display
  (find-possible-field-column-indexes
    (ticket-field-values-to-columns
      (only-valid-tickets
        nearby-tickets
        ticket-rules))
    ticket-rules))
(newline)

