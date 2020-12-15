(load "./lib/alist.scm")
(load "./lib/list.scm")
(load "./lib/parser.scm")
(load "./lib/timings.scm")

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
              mask-bits))))))

(define (ignore-mask-for-address value mask)
  (let ((bits-number
          (string-length mask)))
    (list->string
      (fill-till-length
        '#\0
        bits-number
        (string->list
          (number->string
            (string->number
              value
              10)
            2))))))

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
    address-with-floating-bits))

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
                        (address-to-store-value
                          (address-mask-application
                            (current-instruction 'address)
                            mask)))
                    (evaluation-loop
                      (cdr remaining-instructions)
                      mask
                      (cons
                        (cons
                          address-to-store-value
                          value-to-store)
                        memory))))
                (else (error "Unknown instruction code" current-instruction-code)))))))
  (let ((evaluation-result
          (evaluation-loop instructions initial-mask initial-memory)))
    (alist->list evaluation-result)))

(define (address-combinations-count address)
  (define (loop remaining-bits acc)
    (cond ((null? remaining-bits) acc)
          ((equal? (car remaining-bits) '#\X)
            (loop (cdr remaining-bits) (* 2 acc)))
          (else (loop (cdr remaining-bits) acc))))
  (if (not address) 0
    (loop (string->list address) 1)))

(define (intersection-of-addresses first second)
  (let ((first-bits (string->list first))
        (second-bits (string->list second)))
    (let ((intersection
            (map
              (lambda (p)
                (let ((first-bit (car p))
                      (second-bit (cadr p)))
                  (cond ((equal? first-bit second-bit) first-bit)
                        ((equal? first-bit '#\X) second-bit)
                        ((equal? second-bit '#\X) first-bit)
                        (else #f))))
              (zip
                first-bits
                second-bits))))
      (if (contains? #f intersection) #f
        (list->string intersection)))))

(define (sum-of-set-values memory)
  ;FIXME: The counting of weighted intersections is not mathematically correct and should be improved: use the inclusion/exclusion principle
  ;https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle
  (define (count-weighted-intersections address-to-intersect tail-of-remaining-memory)
    (apply
      +
      (map
        (lambda (tail-address-and-value)
          (let ((tail-address (car tail-address-and-value))
                (tail-value (cdr tail-address-and-value)))
            (*
              (address-combinations-count
                (intersection-of-addresses
                  address-to-intersect
                  tail-address))
              tail-value)))
        tail-of-remaining-memory)))
  (define (count-weighted-combinations remaining-memory acc)
    (if (null? remaining-memory) acc
      (let ((address (car (car remaining-memory)))
            (value (cdr (car remaining-memory))))
        (let ((acc-increment
                (*
                  (address-combinations-count address)
                  value))
              (acc-decrement
                (count-weighted-intersections
                  address
                  (cdr remaining-memory))))
          (count-weighted-combinations
            (cdr remaining-memory)
            (-
              (+
                acc
                acc-increment)
              acc-decrement))))))
  (count-weighted-combinations memory 0))

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
  (with-timings
    (lambda ()
      (sum-of-set-values
        (evaluate
          instructions
          mask
          apply-mask-to-value
          ignore-mask-for-address
          '())))
    write-timings))
(newline)

(newline)
(display "Part 2:")
(newline)
(display
  (with-timings
    (lambda ()
      (sum-of-set-values
        (evaluate
          instructions
          mask
          ignore-mask-for-value
          apply-mask-to-address
          '())))
    write-timings))
(newline)