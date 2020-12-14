(define (alist->list al)
  (define (loop rest-of-al acc)
    (if (null? rest-of-al) (reverse acc)
      (let ((first-binding (car rest-of-al))
            (first-key (car (car rest-of-al))))
          (let ((rest-with-omitted-duplicates
                  (filter
                    (lambda (x)
                      (not
                        (equal?
                          (car x)
                          first-key)))
                    (cdr rest-of-al))))
            (loop
              rest-with-omitted-duplicates
              (cons first-binding acc))))))
  (loop al '()))