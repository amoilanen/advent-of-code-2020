(load "./lib/list.scm")
(load "./lib/parser.scm")
(load "./lib/string.scm")
(load "./lib/timings.scm")

(define input-data "
42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: a
11: 42 31 | 42 11 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: b
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42 | 42 8
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
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
    (cond ((null? remaining-rules) (cons #t offset))
          ((null? remaining-str)
            ((lambda ()
               (newline)
               (display "rule-sequence: ")
               (display (map (lambda (r) (r 'as-list)) remaining-rules))
               (newline)
               (display remaining-str)
               (newline)
               (cons #f offset))))
          (else (let ((next-rule
                        (car remaining-rules)))
                  (let ((next-rule-match ((next-rule 'match) remaining-str)))
                    (let ((next-rule-matches (car next-rule-match))
                          (offset-increment (cdr next-rule-match)))
                      (if next-rule-matches
                        (loop
                          (cdr remaining-rules)
                          (drop remaining-str offset-increment)
                          (+ offset offset-increment))
                        (cons #f offset))))))))
  (define (match str)
    (loop rules str 0))
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
    (cond ((null? remaining-rules) (cons #f 0))
          ((null? str) (cons #f 0))
          (else (let ((next-rule
                        (car remaining-rules)))
                  (let ((next-rule-match ((next-rule 'match) str)))
                    (let ((next-rule-matches (car next-rule-match))
                          (offset (cdr next-rule-match)))
                      (if next-rule-matches
                        (cons #t offset)
                        (loop
                          (cdr remaining-rules)
                          str))))))))
  (define (match str)
    (loop rules str))
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
    (rule-reference "0")))

(define (matching-messages)
  (filter
    (lambda (m)
      (rule-zero
        (string->list m)))
    messages))

(define (number-of-matching-messages)
  (length
    (filter
      (lambda (m)
        (rule-zero
          (string->list m)))
      messages)))

(newline)
(display
  (rule-zero (string->list "aaaabbaaaabbaaa")))
(newline)

(newline)
(display
  (rule-zero (string->list "aaaaabbaabaaaaababaa")))
(newline)

(newline)
(map
  (lambda (m)
    (newline)
    (display m)
    m)
  (matching-messages))
(newline)

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (number-of-matching-messages))
    write-timings))
(newline)
