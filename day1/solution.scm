(load "./lib/tuples.scm")

(define sum-value 2020)
(define input-data (list 1721 979 366 299 675 1456))

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