(define (rule-character ch)
  (lambda (str)
    (cond ((null? str) (cons #f 0))
          ((equal? (car str) ch) (cons #t 1))
          (else (cons #f 0)))))

(define (rule-sequence matchers)
  (define (loop remaining-matchers remaining-str offset)
    (if (null? remaining-matchers) (cons #t offset)
      (let ((next-rule
              (car remaining-matchers)))
        (let ((next-rule-match (next-rule remaining-str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset-increment (cdr next-rule-match)))
            (if next-rule-matches
              (loop
                (cdr remaining-matchers)
                (drop remaining-str offset-increment)
                (+ offset offset-increment))
              (cons #f offset)))))))
  (lambda (str)
    (if (null? str) (cons #f 0)
      (loop matchers str 0))))

(define (rule-one-of matchers)
  (define (loop remaining-matchers str)
    (if (null? remaining-matchers) (cons #f 0)
      (let ((next-rule
              (car remaining-matchers)))
        (let ((next-rule-match (next-rule str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset (cdr next-rule-match)))
            (if next-rule-matches
              (cons #t offset)
              (loop
                (cdr remaining-matchers)
                str)))))))
  (lambda (str)
    (if (null? str) (cons #f 0)
      (loop matchers str))))

(define (rule-reference matcher-idx)
  ;TODO: Implement
  ())

(define input
  (list
    (string->list "abcdef")
    (string->list "bcdef")
    (string->list "bdef")
    (string->list "cdef")
    (string->list "")))

(newline)
(display
  (map
    (rule-character #\a)
    input))
(newline)

(newline)
(display
  (map
    (rule-sequence
      (list
        (rule-character #\b)
        (rule-character #\c)))
    input))
(newline)

(newline)
(display
  (map
    (rule-one-of
      (list
        (rule-character #\b)
        (rule-character #\c)))
    input))
(newline)