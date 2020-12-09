(define input-data "
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
")

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

(define (unique-n-tuples-of elements n)
  (define (element-pairs first rest)
    (if (null? rest)
      '()
      (cons
        (list first (car rest))
        (element-pairs first (cdr rest)))))
  (if (eq? n 1) (map (lambda (x) (list x)) elements)
    (if (null? elements) '()
      (append
        (map
          (lambda (lists) (apply append lists))
          (element-pairs
            (list (car elements))
            (unique-n-tuples-of (cdr elements) (- n 1))))
        (unique-n-tuples-of (cdr elements) n)))))

(define (unique-pairs-of elements)
  (unique-n-tuples-of elements 2))

; Parser
(define (parse-numbers input)
  (let ((numbers-input (split-list-by input '#\newline)))
    (map
      (lambda (x) (string->number (list->string x)))
      (filter
        (lambda (x) (> (length x) 0))
        numbers-input))))

; Solution
(define (allowed-next-numbers preamble)
  (map
    (lambda (p)
      (+ (car p) (cadr p)))
    (unique-pairs-of preamble)))

(define (find-first-invalid-xmas-number xmas-numbers preamble-size)
  (define (find-number remaining-xmas-numbers preamble)
    (if (null? remaining-xmas-numbers) #f
      (let ((current-number (car remaining-xmas-numbers))
            (allowed-numbers (allowed-next-numbers preamble)))
        (if (contains? current-number allowed-numbers)
          (find-number
            (cdr remaining-xmas-numbers)
            (append
              (drop preamble 1)
              (list current-number)))
          current-number))))
  (find-number (drop xmas-numbers preamble-size) (take xmas-numbers preamble-size)))


(newline)
(display "Part 1:")
(newline)
(display
  (find-first-invalid-xmas-number
    (parse-numbers
      (string->list input-data))
    5))
(newline)
