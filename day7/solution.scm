(define input-data "
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
")

(load-option 'regular-expression)

; Utility functions
(define (contains? el list)
  (not (eq? false (member el list))))

(define (split-list-by-list l splitters)
  (define (split l splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((contains? (car l) splitters)
            (split (cdr l) '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) (cons (car l) splitted-part) already-splitted))))
  (reverse (split l '() '())))

(define (split-list-by l el)
  (split-list-by-list l (list el)))

(define (join-strings-with sep strings)
  (if (null? strings) '()
    (substring
      (apply
        string-append
        (map
          (lambda (s)
            (string-append sep s))
          strings))
      1)))

(define (drop-from-tail l num)
  (reverse
    (drop
      (reverse l)
      num)))

(define (omit-empty l)
  (filter
    (lambda (p) (> (length p) 0))
    l))

; Parser
(define (group-into-words input)
  (define (accumulate-words input current-word output)
    (define (append-word current-word output)
      (if (null? current-word)
              output 
              (cons (list->string (reverse current-word)) output)))
    (cond ((null? input)
            (reverse (append-word current-word output)))
          ((equal? '#\space (car input))
            (accumulate-words
              (cdr input)
              '()
              (append-word current-word output)))
          (else
            (accumulate-words
              (cdr input)
              (cons
                (car input)
                current-word)
              output))))
  (accumulate-words input '() '()))

(define (parse-rules input)
  (let ((rule-inputs (split-list-by input '#\newline)))
    (map
      (lambda (rule-input)
        (parse-rule
          (group-into-words rule-input)))
      (omit-empty
          rule-inputs))))

(define (parse-rule input)
  (let ((parts (split-list-by input "contain")))
    (let ((left (car parts))
          (right (cadr parts)))
      (list
        (parse-rule-left-side left)
        (parse-rule-right-side right)))))

(define (parse-rule-left-side input)
    (join-strings-with "_" (drop-from-tail input 1)))

(define (parse-rule-right-side input)
  (if (equal? input (list "no" "other" "bags.")) '()
    (let ((parts (split-list-by-list input (list "bags," "bag," "bag." "bags."))))
      (map
        parse-rule-right-side-part
        (omit-empty
          parts)))))

(define (parse-rule-right-side-part input)
  (list
    (string->number (car input))
    (join-strings-with "_" (cdr input))))

; rule representation

; Solution

(newline)
(display "Part 1:")
(newline)
(display
  (parse-rules
    (string->list input-data)))
