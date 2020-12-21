(load "./lib/list.scm")
(load "./lib/parser.scm")

(define input-data "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
")

; Parser
(define (parse-input input)
  (let ((lines (split-list-by input '#\newline)))
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
  (define (dispatch op)
    (cond ((eq? op 'field-name) field-name)
          ((eq? op 'ranges) ranges)
          ((eq? op 'as-list) (list field-name ranges))
          (else (error "Unsupported op for rule:" op))))
  dispatch)

(define (make-range from to)
  (cons from to))

(define (make-ticket field-values)
  field-values)

; Display the answers

(define ticket-notes
  (parse-input
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (map
    (lambda (x)
      (x 'as-list))
    (ticket-notes 'ticket-rules)))
(newline)
(display
  (ticket-notes 'your-ticket))
(newline)
(display
  (ticket-notes 'nearby-tickets))
(newline)