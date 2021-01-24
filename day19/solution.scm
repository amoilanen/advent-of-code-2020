(load "./lib/list.scm")
(load "./lib/parser.scm")
(load "./lib/string.scm")

(define input-data "
0: 1 2
1: a
2: 1 3 | 3 1
3: b

aab
aba
bab
bba
aaa
abb
aabc
abac
")

; Parser
(define (parse-input input)
  (let ((input-parts
          (omit-empty
            (split-list-by
              (split-list-by
                input
                '#\newline)
                '()))))
    (cons
      (parse-rules (car input-parts))
      (parse-messages (cadr input-parts)))))

(define (parse-rules input)
  (map
    parse-rule
    input))

(define (parse-rule input)
  (let ((rule-parts
         (split-list-by
           input
           #\space)))
    (let ((rule-id
            (char-list->string
                (drop-from-tail (car rule-parts) 1)))
          (rule-definition
            (map char-list->string (cdr rule-parts))))
      (cons rule-id (parse-or rule-definition)))))

(define (parse-or input)
  (let ((or-parts (split-list-by input "|")))
    (if (> (length or-parts) 1)
      (apply
        rule-one-of
        (map
          parse-seq
          or-parts))
      (parse-seq (car or-parts)))))

(define (parse-seq input)
    (if (> (length input) 1)
      (apply
        rule-sequence
        (map
          parse-simple-rule
          input))
      (parse-simple-rule (car input))))

(define (parse-simple-rule input)
  (if (string-is-number? input)
    (rule-reference input)
    (rule-character
      (car
        (string->list input)))))

(define (parse-messages input)
  (map
    list->string
    input))

; Rules
(define (rule-character ch)
  (define (match str)
    (cond ((null? str) (cons #f 0))
          ((equal? (car str) ch) (cons #t 1))
          (else (cons #f 0))))
  (define (dispatch op)
    (cond ((eq? op 'match) match)
          ((eq? op 'as-list) (list "ch:" ch))
          (else (error "Unsupported op for rule-character:" op))))
  dispatch)

(define (rule-sequence . rules)
  (define (loop remaining-rules remaining-str offset)
    (if (null? remaining-rules) (cons #t offset)
      (let ((next-rule
              (car remaining-rules)))
        (let ((next-rule-match ((next-rule 'match) remaining-str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset-increment (cdr next-rule-match)))
            (if next-rule-matches
              (loop
                (cdr remaining-rules)
                (drop remaining-str offset-increment)
                (+ offset offset-increment))
              (cons #f offset)))))))
  (define (match str)
    (if (null? str) (cons #f 0)
      (loop rules str 0)))
  (define (dispatch op)
    (cond ((eq? op 'match) match)
          ((eq? op 'as-list) (list "seq:"
            (map
              (lambda (rule)
                (rule 'as-list))
              rules)))
          (else (error "Unsupported op for rule-sequence:" op))))
  dispatch)

(define (rule-one-of . rules)
  (define (loop remaining-rules str)
    (if (null? remaining-rules) (cons #f 0)
      (let ((next-rule
              (car remaining-rules)))
        (let ((next-rule-match ((next-rule 'match) str)))
          (let ((next-rule-matches (car next-rule-match))
                (offset (cdr next-rule-match)))
            (if next-rule-matches
              (cons #t offset)
              (loop
                (cdr remaining-rules)
                str)))))))
  (define (match str)
    (if (null? str) (cons #f 0)
      (loop rules str)))
  (define (dispatch op)
   (cond ((eq? op 'match) match)
         ((eq? op 'as-list) (list "one-of:"
           (map
             (lambda (rule)
               (rule 'as-list))
             rules)))
         (else (error "Unsupported op for rule-one-of:" op))))
  dispatch)

(define (rule-reference rule-id)
  (define (match str)
    (let ((found-rule (assoc rule-id rules)))
      (if found-rule
        (((cdr found-rule) 'match) str)
        (error "Could not find rule with id" rule-id rules))))
  (define (dispatch op)
   (cond ((eq? op 'match) match)
         ((eq? op 'as-list) (list "ref:" rule-id))
         (else (error "Unsupported op for rule-reference:" op))))
  dispatch)

(define (matches-rule? rule)
  (lambda (str)
    (let ((rule-match-and-offset
            ((rule 'match) str)))
      (let ((rule-match (car rule-match-and-offset))
            (rule-match-offset (cdr rule-match-and-offset)))
        (and
          rule-match
          (=
            rule-match-offset
            (length str)))))))

; Display the results
(define parsed
  (parse-input
    (string->list input-data)))

(define rules
  (car parsed))

(define messages
  (cdr parsed))

(define rule-zero
  (matches-rule?
    (cdr
      (car rules))))

(newline)
(map
  (lambda (m)
    (newline)
    (display m)
    (newline)
    (display
      (rule-zero
        (string->list m))))
  messages)
(newline)