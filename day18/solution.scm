(load "./lib/list.scm")
(load "./lib/parser.scm")
(load "./lib/timings.scm")

(define input "
1 + 2 * 3 + 4 * 5 + 6
1 + (2 * 3) + (4 * (5 + 6))
")

; Parser
(define (parse-input input)
  (let ((lines
          (omit-empty
            (split-list-by input '#\newline))))
    (map
      (lambda (line)
        (strip-spaces
          line))
      lines)))

; Computation
(define ops-with-priorities
  (list
    (list #\^ 4 #f (lambda (x y) (expt x y)))
    (list #\* 3 #t (lambda (x y) (* x y)))
    (list #\/ 3 #t (lambda (x y) (/ x y)))
    (list #\+ 2 #t (lambda (x y) (+ x y)))
    (list #\- 2 #t (lambda (x y) (- x y)))))

(define ops
  (map
    car
    ops-with-priorities))

(define (op-field op field-accessor default-value)
  (let ((found-op
          (assoc op ops-with-priorities)))
    (if found-op
      (field-accessor found-op)
      (default-value))))

(define (op-priority op)
  (op-field op cadr (lambda () -1)))

(define (is-left-associative? op)
  (op-field op caddr (lambda () #f)))

(define (op-lambda op)
  (op-field
    op
    cadddr
    (lambda () (error "Did not find operator configuration" op))))

(define (is-operator? token)
  (contains? token ops))

(define (is-left-bracket? token)
  (equal? token '#\())

(define (is-right-bracket? token)
  (equal? token '#\)))

(define (pop-operators-while operators condition)
  (define (loop remaining-operators popped-operators)
    (if (null? remaining-operators)
      (cons remaining-operators (reverse popped-operators))
      (let ((current-operator (car remaining-operators)))
        (if (condition current-operator)
          (loop
            (cdr remaining-operators)
            (cons current-operator popped-operators))
          (cons remaining-operators (reverse popped-operators))))))
  (loop operators '()))

(define (pop-operators-with-greater-or-equal-precedence operator operators)
  (pop-operators-while
    operators
    (lambda (current-operator)
      (or
        (> (op-priority current-operator) (op-priority operator))
        (and
          (is-left-associative? current-operator)
          (= (op-priority current-operator) (op-priority operator)))))))

(define (pop-upto-left-bracket operators)
  (let ((remaining-operators-and-popped-operators
          (pop-operators-while
            operators
            (lambda (current-operator)
              (not (is-left-bracket? current-operator))))))
      (let ((remaining-operators (car remaining-operators-and-popped-operators))
            (popped-operators (cdr remaining-operators-and-popped-operators)))
        (cons (cdr remaining-operators) popped-operators)))) ; also omit the left bracket itself

; Based on https://en.wikipedia.org/wiki/Shunting-yard_algorithm
; https://brilliant.org/wiki/shunting-yard-algorithm/
(define (to-postfix expr)
  (define (handle-next remaining-tokens operators output)
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

(define (evaluate-postfix expr)
  (define (handle-next remaining-tokens stack)
    (if (null? remaining-tokens)
      (if (> (length stack) 1)
        (error "Invalid expression" expr "remaining stack" stack)
        (car stack))
      (let ((current-token (car remaining-tokens)))
        (cond ((is-operator? current-token)
                (if (< (length stack) 2)
                  (error "Not enough operands on the stack left" stack "remaining tokens" remaining-tokens)
                  (let ((left-operand
                          (car stack))
                        (right-operand
                          (cadr stack))
                        (op-function
                          (op-lambda current-token)))
                    (handle-next
                      (cdr remaining-tokens)
                      (cons
                        (op-function
                          left-operand
                          right-operand)
                        (cddr stack))))))
              (else
                (handle-next
                  (cdr remaining-tokens)
                  (cons
                    (string->number (char->string current-token))
                    stack)))))))
  (handle-next expr '()))

(define (evaluate expr)
  (evaluate-postfix
    (to-postfix expr)))

(define (answer-to-part-1 input-expressions)
  (apply
    +
    (map
      evaluate
      input-expressions)))

; Display results

(define ops-with-priorities
  (list
    (list #\* 2 #t (lambda (x y) (* x y)))
    (list #\+ 2 #t (lambda (x y) (+ x y)))))

(define input-expressions
  (parse-input
   (string->list input)))

(newline)
(display "Part 1:")
(newline)
(display
  (with-timings
    (lambda ()
      (answer-to-part-1
        input-expressions))
    write-timings))
(newline)