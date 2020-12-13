(define (xor x y)
  (and
    (or x y)
    (not (and x y))))