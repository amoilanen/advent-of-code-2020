(load "./lib/list.scm")

(define input-data "
F10
N3
F7
R90
F11
")

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

(define (rotate-waypoint rotation-degrees waypoint-relative-position)
  (let ((angle-degrees (modulo rotation-degrees 360))
        (waypoint-x (car waypoint-relative-position))
        (waypoint-y (cdr waypoint-relative-position)))
    (cond ((equal? angle-degrees 0)
              waypoint-relative-position)
          ((equal? angle-degrees 90)
              (cons (* -1 waypoint-y) waypoint-x))
          ((equal? angle-degrees 180)
              (cons (* -1 waypoint-x) (* -1 waypoint-y)))
          ((equal? angle-degrees 270)
              (cons waypoint-y (* -1 waypoint-x)))
          (else (error "Cannot rotate waypoint by the given number of degrees" angle-degrees waypoint-relative-position)))))

(define (follow-waypoint-instructions instructions position waypoint-relative-position)
  (define (update-waypoint-position instruction waypoint-relative-position)
    (let ((x (car waypoint-relative-position))
          (y (cdr waypoint-relative-position))
          (code (instruction 'code))
          (operand (instruction 'operand)))
      (cond ((equal? (instruction 'code) '#\N)
                (cons x (+ y operand)))
            ((equal? (instruction 'code) '#\S)
                (cons x (- y operand)))
            ((equal? (instruction 'code) '#\W)
                (cons (- x operand) y))
            ((equal? (instruction 'code) '#\E)
                (cons (+ x operand) y))
            ((equal? (instruction 'code) '#\R)
                (rotate-waypoint (* -1 operand) waypoint-relative-position))
            ((equal? (instruction 'code) '#\L)
                (rotate-waypoint operand waypoint-relative-position))
            (else waypoint-relative-position))))
  (define (update-position instruction position waypoint-relative-position)
    (let ((x (car position))
          (y (cdr position))
          (waypoint-x (car waypoint-relative-position))
          (waypoint-y (cdr waypoint-relative-position))
          (code (instruction 'code))
          (operand (instruction 'operand)))
      (cond ((equal? (instruction 'code) '#\F)
                (cons
                  (+ x (* operand waypoint-x))
                  (+ y (* operand waypoint-y))))
            (else position))))
  (if (null? instructions) (cons position waypoint-relative-position)
    (let ((instruction (car instructions)))
      (let ((new-position (update-position instruction position waypoint-relative-position))
            (new-waypoint-relative-position (update-waypoint-position instruction waypoint-relative-position)))
        (follow-waypoint-instructions (cdr instructions) new-position new-waypoint-relative-position)))))

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

(newline)
(display "Part 2:")
(newline)
(display
  (manhattan-distance
    (car
      (follow-waypoint-instructions instructions (cons 0 0) (cons 10 1)))))
(newline)