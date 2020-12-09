(define input-data "
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
")

; Utility functions
(define (contains? el list)
  (not (eq? false (member el list))))

(define (identity x)
  (values x))

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

(define (nth index list)
  (define (nth-loop n l)
    (cond ((null? l) (error "Index out of bounds" index list))
          ((eq? n 0) (car l))
          (else (nth-loop (- n 1) (cdr l)))))
  (if (< index 0) (error "Index out of bounds" index list)
    (nth-loop index list)))

(define (range from to)
  (if (> from to) '()
    (cons from (range (+ 1 from) to))))

; Parser
(define (parse-instructions input)
  (let ((instructions-input (split-list-by input '#\newline)))
    (map
      parse-instruction
      (filter
        (lambda (x) (> (length x) 0))
        instructions-input))))

(define (parse-instruction input)
  (let ((parts (split-list-by input '#\space)))
    (make-instruction
      (list->string
        (car parts))
      (string->number
        (list->string
          (cadr parts))))))

; Instruction
(define (make-instruction opcode operand)
  (define (dispatch op)
    (cond ((eq? op 'opcode) opcode)
          ((eq? op 'operand) operand)
          ((eq? op 'as-list) (list opcode operand))
          (else (error "Unsupported instruction op:" op))))
  dispatch)

(define (make-evaluation-result has-terminated acc-value)
  (define (dispatch op)
    (cond ((eq? op 'has-terminated) has-terminated)
          ((eq? op 'acc-value) acc-value)
          ((eq? op 'as-list) (list has-terminated acc-value))
          (else (error "Unsupported evaluation result op:" op))))
  dispatch)

; Solution
(define (evaluate instructions)
  (define instructions-number (length instructions))
  (define (compute-next-instruction-index instruction instruction-index)
    (if (equal? (instruction 'opcode) "jmp")
       (+ (instruction 'operand) instruction-index)
       (+ 1 instruction-index)))
  (define (compute-next-acc instruction acc)
    (if (equal? (instruction 'opcode) "acc")
       (+ (instruction 'operand) acc)
       acc))
  (define (evaluate-next instruction instruction-index acc history)
    (if (contains? instruction-index history) (make-evaluation-result #f acc)
      (let ((next-instruction-index (compute-next-instruction-index instruction instruction-index))
            (next-acc (compute-next-acc instruction acc)))
        (if (equal? next-instruction-index instructions-number) (make-evaluation-result #t next-acc)
          (evaluate-next
            (nth next-instruction-index instructions)
            next-instruction-index
            next-acc
            (cons instruction-index history))))))
  (evaluate-next (car instructions) 0 0 '()))

(define (generate-corrected-instructions-variants instructions)
  (define (flip-instruction instruction)
    (cond ((equal? (instruction 'opcode) "nop")
            (make-instruction "jump" (instruction 'operand)))
          ((equal? (instruction 'opcode) "jmp")
            (make-instruction "nop" (instruction 'operand)))
          (else #f)))
  (define (try-flipping-instruction-at-index index)
    (let ((instruction (nth index instructions)))
      (let ((flipped-instruction (flip-instruction instruction)))
        (if (not flipped-instruction) #f
          (append
            (take instructions index)
            (list flipped-instruction)
            (drop instructions (+ index 1)))))))
  (let ((indexes
           (range 0 (- (length instructions) 1))))
    (filter
      identity
      (map
        try-flipping-instruction-at-index
        indexes))))

(define (try-correcting-and-find-result-after-termination instructions)
  (let ((all-instruction-variants
          (cons
            instructions
            (generate-corrected-instructions-variants instructions))))
    (let ((all-results
             (map
                evaluate
                all-instruction-variants)))
      (let ((result-with-termination (find (lambda (r) (r 'has-terminated)) all-results)))
        (result-with-termination 'acc-value)))))

(define instructions
  (parse-instructions
        (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display ((evaluate instructions) 'acc-value))
(newline)

(newline)
(display "Part 2:")
(newline)
(display (try-correcting-and-find-result-after-termination instructions))
(newline)