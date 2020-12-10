(define input-data "
149
87
67
45
76
29
107
88
4
11
118
160
20
115
130
91
144
152
33
94
53
148
138
47
104
121
112
116
99
105
34
14
44
137
52
2
65
141
140
86
84
81
124
62
15
68
147
27
106
28
69
163
97
111
162
17
159
122
156
127
46
35
128
123
48
38
129
161
3
24
60
58
155
22
55
75
16
8
78
134
30
61
72
54
41
1
59
101
10
85
139
9
98
21
108
117
131
66
23
77
7
100
51
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

(define (find-jolts adapters)
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

(define (jolt-counts-magic-number jolt-counts)
  (*
    (cdr (assoc 3 jolt-counts))
    (cdr (assoc 1 jolt-counts))))

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

; Inefficient recursive algorithm: fails with "out of memory"! Come up with a more efficient version
(define (adapter-combinations connectivity-list)
  (if (null? connectivity-list) '()
    (let ((first-adapter-connectivity (car connectivity-list))
          (rest-of-connectivity-list (cdr connectivity-list)))
      (let ((adapters-connectable-to-first-adapter (cadr first-adapter-connectivity))
            (first-adapter (car first-adapter-connectivity)))
        (let ((next-adapter-combinations
                 (apply
                   append
                   (map
                     (lambda (next-adapter)
                       (let ((next-connectivity-list
                                (drop-until
                                   (lambda (connectivity)
                                     (equal? (car connectivity) next-adapter))
                                   rest-of-connectivity-list)))
                         (adapter-combinations next-connectivity-list)))
                     adapters-connectable-to-first-adapter))))
          (if (null? next-adapter-combinations) (list first-adapter)
            (map
              (lambda (c)
                (cons first-adapter c))
              next-adapter-combinations)))))))

; Output

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

(newline)
(display "Part 2:")
(newline)
(display
  (length
    (adapter-combinations
      (construct-connectivity-list
        adapters-in-my-bag))))
(newline)