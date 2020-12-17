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

; intersections:
; (list (cons +1 "100X") (cons -1 "1110") (cons +1 "101"))
(define (intersections-with-signs-count intersections)
  (apply +
    (map
      (lambda (intersection)
        (let ((sign (car intersection))
              (address (cdr intersection)))
          (*
            sign
            (address-combinations-count address))))
      intersections)))

; memory:
;(list
;  (list
;    address
;    value
;    (list
;      (cons +1 intersection-3)
;      (cons -1 intersection-3)
;      ...)) ...)
; intersections-to-intersect-with:
;  (list
;      (cons +1 intersection-1)
;      (cons -1 intersection-2))
(define (intersect-remaining-memory-with-intersections memory intersections-to-intersect-with)
  (map
    (lambda (m)
      (let ((address (car m))
            (value (cadr m))
            (intersections-with-signs (caddr m)))
        (list
          address
          value
          (filter
            (lambda (i)
              (cdr i)) ; filter out empty intersections
            (append
              intersections-with-signs
              (map
                (lambda (intersection)
                  (let ((sign (car intersection))
                        (intersection-address (cdr intersection)))
                    (cons
                      (* -1 sign)
                      (intersection-of-addresses intersection-address address))))
                intersections-to-intersect-with))))))
    memory))


(define (sum-of-set-values memory)
  ; remaining-memory:
  ;(list
  ;  (list
  ;    address
  ;    value
  ;    (list
  ;      (cons "+" intersection-1)
  ;      (cons "-" intersection-2)
  ;      ...)) ...)
  (define (count-weighted-combinations remaining-memory acc)
    (if (null? remaining-memory) acc
      (let ((address (car (car remaining-memory)))
            (value (cadr (car remaining-memory)))
            (intersections-with-signs (caddr (car remaining-memory))))
        (let ((acc-increment
                (*
                  (address-combinations-count address)
                  value))
              (acc-increment-correction
                (*
                  (intersections-with-signs-count
                    intersections-with-signs)
                  value))
              (updated-tail-of-remaining-memory
                (intersect-remaining-memory-with-intersections
                  (cdr remaining-memory)
                  (cons (cons +1 address) intersections-with-signs))))
          (count-weighted-combinations
            updated-tail-of-remaining-memory
            (+
              acc
              acc-increment
              acc-increment-correction))))))
  (let ((memory-with-empty-intersection-lists
         (map
          (lambda (c)
            (list (car c) (cdr c) '()))
            memory)))
  (count-weighted-combinations memory-with-empty-intersection-lists 0)))

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

;(define memory
;  (list (cons "X01" 100) (cons "10X" 10) (cons "101" 1)))
;(newline)
;(display
;  (sum-of-set-values memory)) ; // should be 210
;(newline)
;
;(define memory
;  (list (cons "X01" 100) (cons "X11" 10) (cons "0X0" 1)))
;(newline)
;(display
;  (sum-of-set-values memory)) ; // should be 222
;(newline)

;(define intersections
;  (list (cons +1 "X01") (cons -1 "X0X") (cons +1 "010")))
;(newline)
;(display
;  (intersections-with-signs-count
;    intersections)) ; // -1
;(newline)
;
;(define intersections-to-intersect-with
;  (list (cons 1 "XX10") (cons -1 "0X10")))
;(define remaining-memory
;  (list
;    (list "XX1X" 3 (list (cons -1 "X010")))))
;(newline)
;(display
;  (intersect-remaining-memory-with-intersections
;    remaining-memory
;    intersections-to-intersect-with)) ; (list "XX1X" 3 (list (cons -1 "X010") (cons -1 "XX10") (cons 1 "0X10")))
;(newline)