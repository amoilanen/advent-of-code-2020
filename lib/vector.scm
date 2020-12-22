; Try to also make a copy of the vector before?
; What effect on the performance does it have?

(define (vector-set vector index value)
  (vector-set! vector index value)
  vector)