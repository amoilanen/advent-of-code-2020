(define (join-strings-with sep strings)
  (if (null? strings) '()
    (substring
      (apply
        string-append
        (map
          (lambda (s)
            (string-append sep s))
          strings))
      1)))

(define (group-into-words input)
  (define (accumulate-words input current-word output)
    (define (append-word current-word output)
      (if (null? current-word)
              output 
              (cons (list->string (reverse current-word)) output)))
    (cond ((null? input)
            (reverse (append-word current-word output)))
          ((equal? '#\space (car input))
            (accumulate-words
              (cdr input)
              '()
              (append-word current-word output)))
          (else
            (accumulate-words
              (cdr input)
              (cons
                (car input)
                current-word)
              output))))
  (accumulate-words input '() '()))

(define (strip-spaces input)
  (filter
    (lambda (s)
      (not (eq? '#\space s)))
    input))

(define (char-list-to-string chars)
  (apply string-append (map char->string chars)))