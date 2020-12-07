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

; Utility functions
(define (split-list-by l el)
  (define (split l el splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((eq? (car l) el)
            (split (cdr l) el '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) el (cons (car l) splitted-part) already-splitted))))
  (reverse (split l el '() '())))

(define (omit-duplicates l)
  (cond ((null? l) '())
        ((contains? (car l) (cdr l))
          (omit-duplicates (cdr l)))
        (else
          (cons
            (car l)
            (omit-duplicates (cdr l))))))

(define (contains? el list)
  (not (eq? false (member el list))))

; Set operations
(define (union . lists)
  (omit-duplicates (apply append lists)))

(define (intersection . lists)
  (define (intersect-two first second)
    (cond ((null? first)
            '())
          ((contains? (car first) second)
            (cons
              (car first)
              (intersect-two
                (cdr first)
                second)))
          (else (intersect-two
                  (cdr first)
                  second))))
  (cond ((null? lists) '())
        ((equal? (length lists) 1) (car lists))
        (else (let ((first (car lists))
              (rest (cdr lists)))
          (apply
            intersection
            (map
              (lambda (second)
                (intersect-two first second))
              rest))))))

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
