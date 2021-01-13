(define (matches-character ch)
  (lambda (str)
    (cond ((null? str) (cons #f 0))
          ((equal? (car str) ch) (cons #t 1))
          (else (cons #f 0)))))

(define (matches-sequence matchers)
  (lambda (str)
    (if (null? str) (cons #f 0)
        ;TODO: Implement
        (cons #f 0))))

(define (matches-one-of matchers)
  (lambda (str)
    (if (null? str) (cons #f 0)
        ;TODO: Implement
        (cons #f 0))))

(define (matcher-reference matcher-idx)
  ;TODO: Implement
  ())

(newline)
(display
  (map
    (matches-character #\a)
    (list
      (string->list "abcdef")
      (string->list "bcdef")
      (string->list ""))))
(newline)