(load-option 'regular-expression)

(define (has-re-match re input)
  (not
    (equal?
      (re-string-match re input)
      #f)))