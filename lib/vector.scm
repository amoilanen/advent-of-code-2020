; Try to also make a copy of the vector before?
; What effect on the performance does it have?

(define (vector-set v index value)
  (vector-set! v index value)
  v)

(define (vector-omit-index v idx)
  (if (and
        (<= idx (vector-length v))
        (>= idx 0))
    (vector-append
      (vector-head v idx)
      (vector-tail v (+ idx 1)))
    v))

(define (vector-apply op args)
  (apply
    op
    (vector->list args)))