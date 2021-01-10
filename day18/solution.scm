(load "./lib/list.scm")
(load "./lib/parser.scm")

(define input
  (strip-spaces
    (string->list "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")))

(define ops-with-priorities
  '((#\^ 4 #f) (#\* 3 #t) (#\/ 3 #t) (#\+ 2 #t) (#\- 2 #t)))

(define ops
  (map
    car
    ops-with-priorities))

(define (op-priority op)
  (let ((found-op
          (assoc op ops-with-priorities)))
    (if found-op
      (cadr found-op)
      -1)))

(define (is-left-associative? op)
  (let ((found-op
          (assoc op ops-with-priorities)))
    (if found-op
      (caddr found-op)
      #f)))

(define (is-operator? token)
  (contains? token ops))

(define (is-left-bracket? token)
  (equal? token '#\())

(define (is-right-bracket? token)
  (equal? token '#\)))

(define (pop-operators-with-greater-or-equal-precedence operator operators)
  (define (loop remaining-operators popped-operators)
    (if (null? remaining-operators)
      (cons remaining-operators (reverse popped-operators))
      (let ((current-operator (car remaining-operators)))
        (if (or
              (> (op-priority current-operator) (op-priority operator))
              (and
                (is-left-associative? current-operator)
                (= (op-priority current-operator) (op-priority operator))))
          (loop
            (cdr remaining-operators)
            (cons current-operator popped-operators))
          (cons remaining-operators (reverse popped-operators))))))
  (loop operators '()))

(define (pop-upto-left-bracket operators)
  (define (loop remaining-operators popped-operators)
    (if (null? remaining-operators)
      (cons remaining-operators (reverse popped-operators))
      (let ((current-operator (car remaining-operators)))
        (if (is-left-bracket? current-operator)
          (cons (cdr remaining-operators) (reverse popped-operators))
          (loop
            (cdr remaining-operators)
            (cons current-operator popped-operators))))))
  (loop operators '()))

; Based on https://en.wikipedia.org/wiki/Shunting-yard_algorithm
; https://brilliant.org/wiki/shunting-yard-algorithm/
(define (evaluate expr)
  (define (handle-next remaining-tokens operators output)
    ;(newline)
    ;(display "handle-next(")
    ;(display remaining-tokens)
    ;(display ", ")
    ;(display operators)
    ;(display ", ")
    ;(display output)
    ;(display ")")
    ;(newline)
    (if (null? remaining-tokens) (append (reverse output) operators)
      (let ((current-token (car remaining-tokens)))
        (cond ((is-operator? current-token)
                (let ((updated-and-popped-operators
                       (pop-operators-with-greater-or-equal-precedence current-token operators)))
                  (let ((updated-operators (car updated-and-popped-operators))
                        (popped-operators (cdr updated-and-popped-operators)))
                    (handle-next
                      (cdr remaining-tokens)
                      (cons current-token updated-operators)
                      (append popped-operators output)))))
              ((is-left-bracket? current-token)
                (handle-next
                  (cdr remaining-tokens)
                  (cons current-token operators)
                  output))
              ((is-right-bracket? current-token)
                (let ((updated-and-popped-operators
                       (pop-upto-left-bracket operators)))
                  (let ((updated-operators (car updated-and-popped-operators))
                        (popped-operators (cdr updated-and-popped-operators)))
                    (handle-next
                      (cdr remaining-tokens)
                      updated-operators
                      (append popped-operators output)))))
          (else
            (handle-next
              (cdr remaining-tokens)
              operators
              (cons current-token output)))))))
  (handle-next expr '() '()))

(newline)
(display
  (evaluate
    input))
(newline)
