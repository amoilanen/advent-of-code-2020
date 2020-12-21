(load "./lib/list.scm")

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
  input
)

(define (parse-your-ticket input)
  input
)

(define (parse-nearby-tickets input)
  input
)

(define (make-ticket-notes ticket-rules your-ticket nearby-tickets)
  (define (dispatch op)
    (cond ((eq? op 'ticket-rules) ticket-rules)
          ((eq? op 'your-ticket) your-ticket)
          ((eq? op 'nearby-tickets) nearby-tickets)
          ((eq? op 'as-list) (list ticket-rules your-ticket nearby-tickets))
          (else (error "Unsupported op for ticket-notes:" op))))
  dispatch)

; Display the answers

(define ticket-notes
  (parse-input
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (ticket-notes 'nearby-tickets))
(newline)