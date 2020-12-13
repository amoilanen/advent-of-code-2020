(load "./lib/set.scm")
(load "./lib/list.scm")
(load "./lib/parser.scm")

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

; Parser
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
      (make-rule
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
(define (make-rule left right)
  (define bag-colors
    (map
      (lambda (c) (cadr c))
      right))
  (define bag-count
    (apply +
      (map
        (lambda (c) (car c))
        right)))
  (define (dispatch op)
    (cond ((eq? op 'own-color) left)
          ((eq? op 'bag-colors) bag-colors)
          ((eq? op 'contained-bags) right)
          ((eq? op 'as-list) (list left right))
          (else (error "Unsupported rule op:" op))))
  dispatch)

; Solution
(define (bag-colors-containing-transitively rules bag-colors-to-contain)
  (define (bag-colors-containing bag-colors-to-contain)
    (map
      (lambda (r)
        (r 'own-color))
      (filter
        (lambda (rule)
          (have-intersection?
            (rule 'bag-colors)
            bag-colors-to-contain))
        rules)))
  (let ((new-bag-fronteer
           (bag-colors-containing bag-colors-to-contain)))
    (let ((new-bag-colors-to-contain
             (union new-bag-fronteer bag-colors-to-contain)))
      (if (equal? (length new-bag-colors-to-contain) (length bag-colors-to-contain))
         bag-colors-to-contain
         (bag-colors-containing-transitively rules new-bag-colors-to-contain)))))

(define (count-of-bags-transitively-containing rules bag-color)
  (-
    (length
      (bag-colors-containing-transitively rules (list bag-color)))
    1))

(define (number-of-bags-contained rules bag-color)
  (let ((found-rule
           (find
              (lambda (r)
                (equal?
                  (r 'own-color)
                  bag-color))
           rules)))
    (let ((bags (found-rule 'contained-bags)))
      (apply +
        (map
          (lambda (bag)
            (+
              (car bag)
              (*
                (car bag)
                (number-of-bags-contained rules (cadr bag)))))
          bags)))))

(define rules
  (parse-rules
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (count-of-bags-transitively-containing
    rules
    "shiny_gold"))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (number-of-bags-contained
    rules
    "shiny_gold"))
(newline)
