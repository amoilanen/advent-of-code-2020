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
    (ticket-loop tickets value-columns)))

; TODO: Determine the columns name mapping to the column index from the valid tickets
; TODO: Display the answr from the fields of the current ticket

; Display the answers

(define ticket-notes
  (parse-input
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (solution-for-part-1 ticket-notes))
    write-timings))
(newline)

(newline)
(display
  (ticket-field-values-to-columns
    (ticket-notes 'nearby-tickets)))
(newline)

