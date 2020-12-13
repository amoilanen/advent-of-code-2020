(load "./lib/list.scm")
(load "./lib/regex.scm")
(load "./lib/string.scm")

(define input-data "
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
")

; Parser
(define (parse-passports input)
  (let ((passport-inputs (split-list-by (split-list-by input '#\newline) '())))
    (map
      parse-passport
      (filter
        (lambda (p) (> (length p) 0))
        passport-inputs))))

(define (parse-passport passport-input)
  (let ((passport-field-inputs
          (map
            (lambda (l) (split-list-by l '#\space))
            passport-input)))
    (let ((passport-fields (map parse-passport-field (apply append passport-field-inputs))))
      (make-passport passport-fields))))

(define (parse-passport-field passport-field-input)
  (let ((parts (split-list-by passport-field-input '#\:)))
    (make-passport-field
      (list->string (car parts))
      (list->string (cadr parts)))))

; Validation
(define (is-number-in-range? from to input)
  (let ((n (string->number input)))
    (and
      n
      (>= to n)
      (<= from n))))

(define (has-length? len input)
  (equal?
    (string-length input)
    len))

(define (is-valid-byr? input)
  (and
    (has-length? 4 input)
    (is-number-in-range? 1920 2002 input)))

(define (is-valid-iyr? input)
  (and
    (has-length? 4 input)
    (is-number-in-range? 2010 2020 input)))

(define (is-valid-eyr? input)
  (and
    (has-length? 4 input)
    (is-number-in-range? 2020 2030 input)))

(define (is-valid-hgt? input)
  (cond ((string-suffix? "in" input)
          (is-number-in-range? 59 76 (string-drop-suffix input 2)))
        ((string-suffix? "cm" input)
          (is-number-in-range? 150 193 (string-drop-suffix input 2)))
        (else #f)))

(define (is-valid-hcl? input)
  (and
    (has-length? 7 input) ; Scheme regexps seem not to work with the number of repetitions, i.e. {6}
    (has-re-match "^#[0-9a-f]+$" input)))

(define (is-valid-ecl? input)
  (define known-colors
    (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
  (contains? input known-colors))

(define (is-valid-pid? input)
  (and
    (has-length? 9 input)
    (has-re-match "^[0-9]+$" input)))

(define (is-valid-field? passport-field)
  (let ((name (passport-field 'name))
        (value (passport-field 'value)))
    (cond ((equal? name "byr") (is-valid-byr? value))
          ((equal? name "iyr") (is-valid-iyr? value))
          ((equal? name "eyr") (is-valid-eyr? value))
          ((equal? name "hgt") (is-valid-hgt? value))
          ((equal? name "hcl") (is-valid-hcl? value))
          ((equal? name "ecl") (is-valid-ecl? value))
          ((equal? name "pid") (is-valid-pid? value))
          (else #t))))

; Passport definition
(define (make-passport-field name value)
  (define (dispatch op)
    (cond ((eq? op 'name) name)
          ((eq? op 'value) value)
          ((eq? op 'as-list) (list name value))
          (else (error "Unsupported password-field op:" op))))
  dispatch
)

(define (make-passport passport-fields)
  (define required-field-names (list 'byr 'iyr 'eyr 'hgt 'hcl 'ecl 'pid))
  (define (has-field? field-name)
    (some?
      (lambda (field)
        (equal? (field 'name) (symbol->string field-name)))
      passport-fields))
  (define (has-required-fields?)
    (every?
      has-field?
      required-field-names))
  (define (are-fields-valid?)
    (every? is-valid-field? passport-fields))
  (define (is-valid?)
    (and
      (has-required-fields?)
      (are-fields-valid?)))
  (define as-list
    (map
      (lambda (f) (f 'as-list))
      passport-fields))
  (define (dispatch op)
    (cond ((eq? op 'is-valid) is-valid?)
          ((eq? op 'has-required-fields) has-required-fields?)
          ((eq? op 'has-field) has-field?)
          ((eq? op 'as-list) as-list)
          (else (error "Unsupported password-field op:" op))))
  dispatch
)

; Solution

(define passports
  (parse-passports
    (string->list input-data)))

(define naive-passport-validity
  (map
    (lambda (p) ((p 'has-required-fields)))
    passports))

(define naively-valid-passports-number
  (length
    (filter
      (lambda (x) x)
      naive-passport-validity)))

(newline)
(display "Part 1:")
(newline)
(display naively-valid-passports-number)
(newline)

(define passport-validity
  (map
    (lambda (p) ((p 'is-valid)))
    passports))

(define valid-passports-number
  (length
    (filter
      (lambda (x) x)
      passport-validity)))

(newline)
(display "Part 2:")
(newline)
(display valid-passports-number)
(newline)