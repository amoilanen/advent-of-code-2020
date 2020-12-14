(load "./lib/alist.scm")
(load "./lib/list.scm")
(load "./lib/parser.scm")

(define input-data "
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
")

; Parser
(define (parse-instructions input)
  (let ((lines
          (omit-empty
            (split-list-by
              (strip-spaces input)
              '#\newline))))
    (map
      parse-instruction
      lines)))

(define (parse-instruction input)
  (let ((input-parts (split-list-by-list input (string->list "=[]"))))
    (let ((instruction
            (map
              list->string
                (omit-empty
                  input-parts))))
      (let ((op-code (car instruction)))
        (cond ((equal? op-code "mask")
                 (op-set-mask (cadr instruction)))
              (else (apply op-memory-write (cdr instruction))))))))

; Parsed instructions
(define (op-set-mask mask)
  (define (dispatch op)
    (cond ((eq? op 'mask) mask)
          ((eq? op 'code) "mask")
          ((eq? op 'as-list) (list "mask" mask))
          (else (error "Unsupported instruction op-set-mask:" op))))
  dispatch)

(define (op-memory-write address value)
  (define (dispatch op)
    (cond ((eq? op 'address) address)
          ((eq? op 'value) value)
          ((eq? op 'code) "mem")
          ((eq? op 'as-list) (list "mem" address value))
          (else (error "Unsupported instruction op-memory-write:" op))))
  dispatch)

; Solution
(define (apply-mask value mask)
  (let ((binary-value
          (number->string
            (string->number value 10)
            2))
        (bits-number
          (string-length mask)))
    (let ((binary-value-bits
            (fill-till-length
              '#\0
              bits-number
              (string->list binary-value)))
          (mask-bits
            (string->list mask)))
      (string->number
        (list->string
          (map
            (lambda (x)
              (let ((value-bit (car x))
                    (mask-bit (cadr x)))
                (if (equal? mask-bit '#\X)
                  value-bit
                  mask-bit)))
            (zip
              binary-value-bits
              mask-bits)))
        2))))

(define (evaluate instructions initial-mask initial-memory)
  (define (evaluation-loop remaining-instructions mask memory)
    (if (null? remaining-instructions) memory
      (let ((current-instruction (car remaining-instructions)))
        (let ((current-instruction-code (current-instruction 'code)))
          (cond ((equal? current-instruction-code "mask")
                   (evaluation-loop
                     (cdr remaining-instructions)
                     (current-instruction 'mask)
                     memory))
                ((equal? current-instruction-code "mem")
                  (let ((address (current-instruction 'address))
                        (value-to-store
                          (apply-mask
                            (current-instruction 'value)
                            mask)))
                    (evaluation-loop
                      (cdr remaining-instructions)
                      mask
                      (cons
                        (cons
                          address
                          value-to-store)
                        memory))))
                (else (error "Unknown instruction code" current-instruction-code)))))))
  (let ((evaluation-result
          (evaluation-loop instructions initial-mask initial-memory)))
    (alist->list evaluation-result)))

(define (part1-solution memory)
  (apply
    +
    (map
      (lambda (x)
        (cdr x))
      memory)))

(define instructions
  (parse-instructions
    (string->list input-data)))

(define mask
  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

; Executing solutions
(newline)
(display "Part 1:")
(newline)
(display
  (part1-solution
    (evaluate
      instructions
      mask
      '())))
(newline)