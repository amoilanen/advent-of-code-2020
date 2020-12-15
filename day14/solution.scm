(load "./lib/alist.scm")
(load "./lib/list.scm")
(load "./lib/parser.scm")

(define input-data "
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
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
(define (apply-mask value mask apply-mask-bit)
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
        (list->string
          (map
            (lambda (x)
              (let ((value-bit (car x))
                    (mask-bit (cadr x)))
                (apply-mask-bit value-bit mask-bit)))
            (zip
              binary-value-bits
              mask-bits)))
        )))

(define (ignore-mask-for-address value mask)
  (list value))

(define (ignore-mask-for-value value mask)
  (string->number value 10))

(define (apply-mask-to-value value mask)
  (string->number
    (apply-mask
      value
      mask
      apply-mask-bit-ignore-x)
    2))

(define (apply-mask-to-address address mask)
  (let ((address-with-floating-bits
          (apply-mask
            address
            mask
            apply-mask-bit-ignore-0)))
    (all-addresses
      address-with-floating-bits)))

(define (all-addresses floating-bits-address)
  (define (loop remaining-bits acc)
    (if (null? remaining-bits) acc
      (let ((next-bit (car remaining-bits)))
        (let ((updated-acc
                (if (equal? next-bit '#\X)
                  (append
                    (map
                      (lambda (l)
                        (cons '#\0 l))
                      acc)
                    (map
                      (lambda (l)
                        (cons '#\1 l))
                      acc))
                  (map
                    (lambda (l)
                        (cons next-bit l))
                    acc)
                )))
          (loop (cdr remaining-bits) updated-acc)))))
  (map
    (lambda (l)
      (list->string
        (reverse l)))
    (loop (string->list floating-bits-address) (list '()))))

(define (apply-mask-bit-ignore-x value-bit mask-bit)
  (if (equal? mask-bit '#\X)
    value-bit
    mask-bit))

(define (apply-mask-bit-ignore-0 value-bit mask-bit)
  (if (equal? mask-bit '#\0)
    value-bit
    mask-bit))

(define (evaluate instructions initial-mask value-mask-application address-mask-application initial-memory)
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
                  (let ((value-to-store
                          (value-mask-application
                            (current-instruction 'value)
                            mask))
                        (addresses-to-store-value
                          (address-mask-application
                            (current-instruction 'address)
                            mask)))
                    (evaluation-loop
                      (cdr remaining-instructions)
                      mask
                      (append
                        (map
                          (lambda (address)
                            (cons
                              address
                              value-to-store))
                          addresses-to-store-value)
                        memory))))
                (else (error "Unknown instruction code" current-instruction-code)))))))
  (let ((evaluation-result
          (evaluation-loop instructions initial-mask initial-memory)))
    (alist->list evaluation-result)))

(define (sum-of-set-values memory)
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
  (sum-of-set-values
    (evaluate
      instructions
      mask
      apply-mask-to-value
      ignore-mask-for-address
      '())))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (sum-of-set-values
    (evaluate
      instructions
      mask
      ignore-mask-for-value
      apply-mask-to-address
      '())))
(newline)