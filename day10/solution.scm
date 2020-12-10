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

(define (take-at-most l n)
  (if (> n (length l))
    l
    (take l n)))

(define (drop-until p l)
  (cond ((null? l) '())
        ((p (car l)) l)
        (else (drop-until
                p
                (cdr l)))))

; Parser
(define (parse-numbers input)
  (let ((numbers-input (split-list-by input '#\newline)))
    (map
      (lambda (x) (string->number (list->string x)))
      (filter
        (lambda (x) (> (length x) 0))
        numbers-input))))

; Solution
(define (sorted-full-chain adapters)
  (let ((max-adapter (apply max adapters)))
    (let ((device (+ max-jolt-difference max-adapter)))
      (let ((full-chain (append (list 0 device) adapters)))
        (sort
           full-chain
           (lambda (x y) (< x y)))))))

(define (jolt-pairs adapters)
  (let ((full-chain (sorted-full-chain adapters)))
    (let ((adapter-pairs (zip full-chain (cdr full-chain))))
      (map
         (lambda (p)
            (- (cadr p) (car p)))
            adapter-pairs))))

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

(define (jolt-counts-magic-number adapters)
  (let ((counts
           (jolt-counts
             (jolt-pairs
               adapters))))
    (*
      (cdr (assoc 3 counts))
      (cdr (assoc 1 counts)))))

(define (construct-connectivity-list adapters)
  (let ((full-chain (sorted-full-chain adapters)))
    (define (process-next-adapter remaining-adapters connectivity-list)
      (if (null? remaining-adapters) connectivity-list
        (let ((current-adapter (car remaining-adapters)))
          (let ((connectable-adapters
            (filter
              (lambda (a)
                (<= (- a current-adapter) max-jolt-difference))
              (take-at-most (cdr remaining-adapters) max-jolt-difference))))
            (process-next-adapter
              (cdr remaining-adapters)
              (append
                connectivity-list
                (list (list current-adapter connectable-adapters))))))))
    (process-next-adapter full-chain '())))

(define (compute-number-of-combinations-for-all connectivity-list)
  (define (compute-number-of-combinations-from-known remaining-connectivity-list already-computed-numbers)
    (define (get-already-computed-number adapter)
      (cdr
        (find
          (lambda (computed-number)
            (equal? adapter (car computed-number)))
          already-computed-numbers)))
    (if (null? remaining-connectivity-list) already-computed-numbers
      (let ((next-adapter-connectivity (car remaining-connectivity-list))
            (rest-of-connectivity-list (cdr remaining-connectivity-list)))
        (let ((next-adapter (car next-adapter-connectivity))
              (adapters-connectable-from-next-adapter (cadr next-adapter-connectivity)))
          (let ((number-of-combinations-for-next-adapter
                   (apply
                     +
                     (map
                       get-already-computed-number
                       adapters-connectable-from-next-adapter))))
            (compute-number-of-combinations-from-known
              (cdr remaining-connectivity-list)
              (cons
                (cons next-adapter number-of-combinations-for-next-adapter)
                already-computed-numbers)))))))
  (let ((reversed-connectivity-list (reverse connectivity-list)))
    (compute-number-of-combinations-from-known
      (cdr reversed-connectivity-list)
      (list (cons (car (car reversed-connectivity-list)) 1)))))

(define (combinations-number adapters)
  (cdr
    (car
      (compute-number-of-combinations-for-all
        (construct-connectivity-list adapters)))))

; Output

(define adapters-in-my-bag
  (parse-numbers
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (jolt-counts-magic-number
    adapters-in-my-bag))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (combinations-number
      adapters-in-my-bag))
(newline)