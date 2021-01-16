(define (rule-character ch)
  (lambda (str)
    (cond ((null? str) (cons #f 0))
          ((equal? (car str) ch) (cons #t 1))
          (else (cons #f 0)))))

(define (rule-sequence . rules)
  (define (loop remaining-rules remaining-str offset)
    (if (null? remaining-rules) (cons #t offset)
      (let ((next-rule
              (car remaining-rules)))
        (let ((next-rule-match (next-rule remaining-str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset-increment (cdr next-rule-match)))
            (if next-rule-matches
              (loop
                (cdr remaining-rules)
                (drop remaining-str offset-increment)
                (+ offset offset-increment))
              (cons #f offset)))))))
  (lambda (str)
    (if (null? str) (cons #f 0)
      (loop rules str 0))))

(define (rule-one-of . rules)
  (define (loop remaining-rules str)
    (if (null? remaining-rules) (cons #f 0)
      (let ((next-rule
              (car remaining-rules)))
        (let ((next-rule-match (next-rule str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset (cdr next-rule-match)))
            (if next-rule-matches
              (cons #t offset)
              (loop
                (cdr remaining-rules)
                str)))))))
  (lambda (str)
    (if (null? str) (cons #f 0)
      (loop rules str))))

(define (rule-reference rule-id)
  (lambda (str)
    (let ((found-rule (assoc rule-id rules)))
      (if found-rule
        ((cdr found-rule) str)
        (error "Could not find rule with id" rule-id rules)))))

;0: 1 2
;1: "a"
;2: 1 3 | 3 1
;3: "b"
(define rules
  (list
    (cons 0 (rule-sequence
         (rule-reference 1)
         (rule-reference 2)))
    (cons 1 (rule-character #\a))
    (cons 2 (rule-one-of
         (rule-sequence
           (rule-reference 1)
           (rule-reference 3))
         (rule-sequence
           (rule-reference 3)
           (rule-reference 1))))
    (cons 3 (rule-character #\b))))

(define rule-zero
  (rule-reference 0))

(define input
  (list
    (string->list "aab")
    (string->list "aba")
    (string->list "bab")
    (string->list "bba")
    (string->list "aaa")
    (string->list "abb")))

(newline)
(display
  (map
    rule-zero
    input))
(newline)