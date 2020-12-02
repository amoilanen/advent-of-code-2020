(define sum-value 2020)
(define input-data (list 1721 979 366 299 675 1456))

(define (element-pairs first rest)
  (if (null? rest)
    '()
    (cons
      (list first (car rest))
      (element-pairs first (cdr rest)))))

(define (unique-n-tuples-of elements n)
  (if (eq? n 1) (map (lambda (x) (list x)) elements)
    (if (null? elements) '()
      (append
        (map
          (lambda (lists) (apply append lists))
          (element-pairs
            (list (car elements))
            (unique-n-tuples-of (cdr elements) (- n 1))))
        (unique-n-tuples-of (cdr elements) n)))))

(define (tuples-which-sum-to tuples sum-value)
  (filter
    (lambda (l)
      (eq?
        (apply + l)
        sum-value))
    tuples))

(define tuple-sizes (list 2 3))

(map
  (lambda (tuple-size)
    (let ((found-tuples (tuples-which-sum-to (unique-n-tuples-of input-data tuple-size) sum-value)))
      (if (null? found-tuples) (error "Could not find a tuple which sums to value" input-data sum-value tuple-size)
        (let ((answer (apply * (car found-tuples))))
          (newline)
          (display tuple-size)
          (newline)
          (display answer)
        ))))
  tuple-sizes)