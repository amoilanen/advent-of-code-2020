(load "./lib/set.scm")
(load "./lib/list.scm")

(define input-data "
abc

a
b
c

ab
ac

a
a
a
a

b
")

; Parser
(define (parse-groups input)
  (let ((group-inputs (split-list-by (split-list-by input '#\newline) '())))
    (map
      parse-group
      (filter
        (lambda (p) (> (length p) 0))
        group-inputs))))

(define (parse-group input)
  input)

(define (total-number-of-yes group-questions-answered-yes groups)
  (apply + (map
              (lambda (g)
                (length (group-questions-answered-yes g)))
              groups)))

(define groups
  (parse-groups
      (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (total-number-of-yes
    (lambda (g)
      (apply union g))
    groups))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (total-number-of-yes
    (lambda (g)
      (apply intersection g))
    groups))
(newline)
