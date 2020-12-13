(load "./lib/boolean.scm")
(load "./lib/list.scm")
(load "./lib/parser.scm")

(define input-data "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

; Parser
; Scheme also has specialized Parser language https://www.gnu.org/software/mit-scheme/documentation/testing/mit-scheme-ref/Parser-Language.html
; which we might had used here instead
(define (parse-password-records input)
  (let ((lines (split-list-by input '#\newline)))
    (map parse-password-and-rule lines)))

(define (parse-password-and-rule input)
  (let ((parts (split-list-by input '#\:)))
    (let ((rule (parse-password-rule (car parts)))
          (password (parse-password (cadr parts))))
      (password-record rule password))))

(define (parse-password-rule input)
  (let ((parts (split-list-by input '#\space)))
    (let ((left-side (car parts))
          (right-side (cadr parts)))
      (let ((rule-left-and-rule-right (split-list-by left-side '#\-)))
        (let ((rule-left (string->number (char-list-to-string (car rule-left-and-rule-right))))
              (rule-right (string->number (char-list-to-string (cadr rule-left-and-rule-right))))
              (char (car right-side)))
          (password-rule rule-left rule-right char))))))

(define (parse-password input)
  (strip-spaces input))

; Definitions of password record and password rule
(define (password-rule rule-left rule-right char)
  (list rule-left rule-right char))
(define (password-rule-rule-left rule) (car rule))
(define (password-rule-rule-right rule) (cadr rule))
(define (password-rule-char rule) (caddr rule))

(define (password-record rule password)
  (list rule password))
(define (password-record-rule record) (car record))
(define (password-record-password record) (cadr record))

(define (old-password-policy password-record)
  (let ((rule-left (password-rule-rule-left (password-record-rule  password-record)))
        (rule-right (password-rule-rule-right (password-record-rule  password-record)))
        (char (password-rule-char (password-record-rule  password-record)))
        (password (password-record-password  password-record)))
    (let ((char-occurences (element-occurences-count char password)))
      (and (<= char-occurences rule-right) (>= char-occurences rule-left)))))

(define (new-password-policy password-record)
  (let ((rule-left (password-rule-rule-left (password-record-rule  password-record)))
        (rule-right (password-rule-rule-right (password-record-rule  password-record)))
        (char (password-rule-char (password-record-rule  password-record)))
        (password (password-record-password  password-record)))
    (let ((first-char (nth (- rule-left 1) password))
          (second-char (nth (- rule-right 1) password)))
      (xor (eq? first-char char) (eq? second-char char)))))

(define (filter-valid-passwords passwords policy)
  (filter
    (lambda (x) x)
    (map policy passwords)))

(newline)
(let ((passwords (parse-password-records
                   (string->list input-data))))
  (newline)
  (display "part 1:")
  (newline)
  (display
    (length
      (filter-valid-passwords passwords old-password-policy)))
  (newline)
  (display "part 2:")
  (newline)
  (display
    (length
      (filter-valid-passwords passwords new-password-policy))))