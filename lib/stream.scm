; Stream implementation
(define (force x)
  (x))

(define (stream-numbers-from n)
  (cons
    n
    (lambda () (stream-numbers-from (+ n 1)))))

(define (stream-multiples-of n)
  (stream-map
    (lambda (x) (* n x))
    (stream-numbers-from 1)))

(define (stream-constant-of x)
  (cons
    x
    (lambda () (stream-constant-of x))))

(define (stream-take-until predicate stream)
  (define (loop predicate accumulated rest)
    (let ((next-value (car rest)))
      (if (predicate next-value)
        (reverse (cons next-value accumulated))
        (loop predicate (cons next-value accumulated) (force (cdr rest))))))
  (loop predicate '() stream))

(define (stream-find predicate stream)
  (define (loop predicate rest)
    (let ((next-value (car rest)))
      (if (predicate next-value)
        next-value
        (loop predicate (force (cdr rest))))))
  (loop predicate stream))

(define (stream-take stream n)
  (define (loop rest remaining-n accumulated)
    (if (<= remaining-n 0) (reverse accumulated)
      (let ((next-value (car rest)))
        (loop (force (cdr rest)) (- remaining-n 1) (cons next-value accumulated)))))
  (loop stream n '()))

(define (stream-drop stream n)
  (define (loop rest remaining-n)
    (if (<= remaining-n 0) rest
        (loop (force (cdr rest)) (- remaining-n 1))))
  (loop stream n))

(define (stream-map f stream)
  (cons
    (f (car stream))
    (lambda ()
      (stream-map
        f
        (force (cdr stream))))))

(define (stream-zip . streams)
  (define (stream-zip-two s1 s2)
    (let ((first (car s1))
          (second (car s2)))
      (cons
        (cons first second)
        (lambda ()
          (stream-zip-two
            (force (cdr s1))
            (force (cdr s2)))))))
  (define (loop rest-of-streams result)
    (if (null? rest-of-streams) result
      (let ((first-stream (car rest-of-streams)))
        (loop (cdr rest-of-streams) (stream-zip-two first-stream result)))))
  (loop (reverse streams) (stream-constant-of '())))

(define (stream-merge-ordered ordering . streams)
  (define (stream-merge-two-ordered ordering s1 s2)
    (let ((first (car s1))
          (second (car s2)))
      (if (ordering first second)
        (cons
          first
          (lambda ()
            (stream-merge-two-ordered
              ordering
              (force (cdr s1))
              s2)))
        (cons
          second
          (lambda ()
            (stream-merge-two-ordered
               ordering
               s1
               (force (cdr s2))))))))
  (define (loop rest-of-streams result)
    (if (null? rest-of-streams) result
      (let ((first-stream (car rest-of-streams)))
        (loop (cdr rest-of-streams) (stream-merge-two-ordered ordering first-stream result)))))
  (loop (cdr streams) (car streams)))

(define (stream-find-window window-length predicate stream)
  (define (find-next window s)
    (if (predicate window) window
      (find-next
        (append (drop window 1) (list (car s)))
        (force (cdr s)))))
  (find-next (stream-take stream window-length) (stream-drop stream window-length)))