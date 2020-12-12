(define input-data "
F10
N3
F7
R90
F11
")

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

; Parser
(define (parse-instructions input)
  (let ((instructions-input (split-list-by input '#\newline)))
    (map
      parse-instruction
      (filter
        (lambda (x) (> (length x) 0))
        instructions-input))))

(define (parse-instruction input)
  (let ((instruction-code (car input))
        (instruction-operand
           (string->number
              (list->string
                 (cdr input)))))
    (make-instruction
      instruction-code
      instruction-operand)))

; Instruction
(define (make-instruction code operand)
  (define (dispatch op)
    (cond ((eq? op 'code) code)
          ((eq? op 'operand) operand)
          ((eq? op 'as-list) (list code operand))
          (else (error "Unsupported instruction op:" op))))
  dispatch)

; Solution
(define (angle-to-direction angle-degrees)
  ;direction-x = cos(angle-degrees)
  ;direction-y= sin(angle-degrees)
  (cond ((equal? angle-degrees 0) (cons 1 0))
        ((equal? angle-degrees 90) (cons 0 1))
        ((equal? angle-degrees 180) (cons -1 0))
        ((equal? angle-degrees 270) (cons 0 -1))
        (else (error "Cannot translate degrees to direction" angle-degrees))))

(define (follow-instructions instructions position angle-degrees)
  (define (update-position instruction position)
    (let ((code (instruction 'code))
          (operand (instruction 'operand)))
      (cond ((equal? (instruction 'code) '#\N)
                (cons (car position) (+ (cdr position) operand)))
            ((equal? (instruction 'code) '#\S)
                (cons (car position) (- (cdr position) operand)))
            ((equal? (instruction 'code) '#\W)
                (cons (- (car position) operand) (cdr position)))
            ((equal? (instruction 'code) '#\E)
                (cons (+ (car position) operand) (cdr position)))
            ((equal? (instruction 'code) '#\F)
                (let ((direction (angle-to-direction angle-degrees)))
                  (cons
                    (+ (car position)
                       (* operand
                         (car direction)))
                    (+ (cdr position)
                       (* operand
                         (cdr direction))))))
            (else position))))
  (define (update-angle instruction angle-degrees)
    (let ((code (instruction 'code))
          (operand (instruction 'operand)))
      (cond ((equal? (instruction 'code) '#\L)
                (modulo (+ angle-degrees operand) 360))
            ((equal? (instruction 'code) '#\R)
                (modulo (- angle-degrees operand) 360))
            (else angle-degrees))))
  (if (null? instructions) (cons position angle-degrees)
    (let ((instruction (car instructions)))
      (let ((new-position (update-position instruction position))
            (new-angle-degrees (update-angle instruction angle-degrees)))
        (follow-instructions (cdr instructions) new-position new-angle-degrees)))))

(define (manhattan-distance position)
  (+ (car position) (* -1 (cdr position))))

; Display
(define instructions
  (parse-instructions
    (string->list input-data)))

(newline)
(display "Part 1:")
(newline)
(display
  (manhattan-distance
    (car
      (follow-instructions instructions (cons 0 0) 0))))
(newline)
