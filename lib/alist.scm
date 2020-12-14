(define (alist->list al)
  (define (loop rest-of-al)
    (if (null? rest-of-al) '()
      (let ((first-binding (car rest-of-al))
            (first-key (car (car rest-of-al))))
        (let ((rest-contains-first-key
                (find
                  (lambda (x)
                    (equal?
                      (car x)
                      first-key))
                  (cdr rest-of-al))))
          (if rest-contains-first-key
            (alist->list (cdr rest-of-al))
            (cons
              first-binding
              (alist->list (cdr rest-of-al))))))))
  (loop (reverse al)))