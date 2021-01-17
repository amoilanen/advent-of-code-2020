(load "./lib/list.scm")
(load "./lib/parser.scm")

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
    ;TODO: parse the rules
    rule-parts))

(define (parse-messages input)
  (map
    list->string
    input))

(define (parse-instruction input)
  (let ((input-parts (split-list-by-list input (string->list "=[]"))))
    (let ((instruction
            (map
              list->string
                (omit-empty
                  input-parts))))
      (let ((op-code (car instruction)))
        (cond ((equal? op-code "mask")
                 (op-set-mask (cadr instruction)))
              (else (apply op-memory-write (cdr instruction))))))))

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

; Display the results

;(newline)
;(display
;  (parse-input
;    (string->list input-data)))
;(newline)

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


(define input
  (list
    (string->list "aab")
    (string->list "aba")
    (string->list "bab")
    (string->list "bba")
    (string->list "aaa")
    (string->list "abb")))


(define rule-zero
  (rule-reference 0))

;TODO: Also visualize the rules themselves
(newline)
(display
  (map
    (rule-zero 'match)
    input))
(newline)

(newline)
(display
  (map
    (lambda (r)
      ((cdr r) 'as-list))
    rules))
(newline)