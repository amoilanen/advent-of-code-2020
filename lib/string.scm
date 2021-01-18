(define (string-drop-suffix input suffix-len)
  (list->string
    (reverse
      (drop
        (reverse
          (string->list input))
        suffix-len))))

(define (char-list->string chars)
  (apply
    string-append
    (map
      char->string
      chars)))

(define (string-is-number? str)
  (if (string->number str) #t #f))