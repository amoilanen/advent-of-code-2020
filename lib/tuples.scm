(define (unique-n-tuples-of elements n)
  (define (element-pairs first rest)
    (if (null? rest)
      '()
      (cons
        (list first (car rest))
        (element-pairs first (cdr rest)))))
  (if (eq? n 1) (map (lambda (x) (list x)) elements)
    (if (null? elements) '()
      (append
        (map
          (lambda (lists) (apply append lists))
          (element-pairs
            (list (car elements))
            (unique-n-tuples-of (cdr elements) (- n 1))))
        (unique-n-tuples-of (cdr elements) n)))))

(define (unique-pairs-of elements)
  (unique-n-tuples-of elements 2))