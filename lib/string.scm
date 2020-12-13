(define (string-drop-suffix input suffix-len)
  (list->string
    (reverse
      (drop
        (reverse
          (string->list input))
        suffix-len))))