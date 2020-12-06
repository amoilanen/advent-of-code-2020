(define input-data "
FFFBBFBLLR
BFBBBFFRLR
BFBBBBFLRR
")

; Utility functions
(define (split-list-by l el)
  (define (split l el splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((eq? (car l) el)
            (split (cdr l) el '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) el (cons (car l) splitted-part) already-splitted))))
  (reverse (split l el '() '())))

; Parser
(define (parse-passes input)
  (let ((passes-inputs
          (split-list-by input '#\newline)))
    (map
      parse-pass
      (filter
        (lambda (p) (> (length p) 0))
        passes-inputs))))

(define (parse-pass input)
  (define (list-of-zero-ones->number l)
    (string->number
      (list->string l)
      2))
  (define (replace-character ch)
    (cond ((equal? ch '#\B) '#\1)
          ((equal? ch '#\F) '#\0)
          ((equal? ch '#\R) '#\1)
          ((equal? ch '#\L) '#\0)
          (else ch)))
  (define (replace-characters input output)
    (if (null? input)
      (list-of-zero-ones->number
        (reverse output))
      (replace-characters
        (cdr input)
        (cons
          (replace-character (car input))
          output))))
  (replace-characters input '())
)

; Finding the gap in the scanned passes
(define (find-missing-pass passes)
  (define (has-one-element-gap pair)
    (equal? (cadr pair) (+ 2 (car pair))))
  (let ((sorted-passes
           (sort passes (lambda (x y) (< x y)))))
    (let ((pairs
             (zip sorted-passes (cdr sorted-passes))))
      (let ((fitting-pairs
               (filter has-one-element-gap pairs)))
        (if (null? fitting-pairs) (error "Did not find a gap for a pass")
          (+ (car (car fitting-pairs)) 1))))))

; Solution

(define passes
  (parse-passes
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (apply max passes))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (find-missing-pass passes))