(define input-data "
16
10
15
5
1
11
7
19
6
12
4
")

(define max-jolt-difference 3)

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

; Parser
(define (parse-numbers input)
  (let ((numbers-input (split-list-by input '#\newline)))
    (map
      (lambda (x) (string->number (list->string x)))
      (filter
        (lambda (x) (> (length x) 0))
        numbers-input))))

(define (find-jolts adapters)
  (let ((max-adapter (apply max adapters)))
    (let ((device (+ max-jolt-difference max-adapter)))
      (let ((full-chain (append (list 0 device) adapters)))
        (let ((sorted-full-chain
                 (sort
                   full-chain
                   (lambda (x y) (< x y))
                 )))
          (let ((adapter-pairs (zip sorted-full-chain (cdr sorted-full-chain))))
            (map
              (lambda (p)
                (- (cadr p) (car p)))
              adapter-pairs)))))))

(define (jolt-counts jolts)
  (define (count-next-jolt rest-of-jolts frequencies)
    (if (null? rest-of-jolts) frequencies
      (let ((current-jolt (car rest-of-jolts)))
        (let ((current-jolt-frequency (assoc current-jolt frequencies)))
          (let ((current-jolt-count
            (if current-jolt-frequency (cdr current-jolt-frequency) 0)))
            (count-next-jolt
              (cdr rest-of-jolts)
              (cons
                (cons current-jolt (+ 1 current-jolt-count))
                frequencies)))))))
  (count-next-jolt jolts '()))

(define (jolt-counts-magic-number jolt-counts)
  (*
    (cdr (assoc 3 jolt-counts))
    (cdr (assoc 1 jolt-counts))))

(define adapters-in-my-bag
  (parse-numbers
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (jolt-counts-magic-number
    (jolt-counts
      (find-jolts
        adapters-in-my-bag))))
(newline)