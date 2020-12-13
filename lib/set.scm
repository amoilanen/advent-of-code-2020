(define (omit-duplicates l)
  (cond ((null? l) '())
        ((contains? (car l) (cdr l))
          (omit-duplicates (cdr l)))
        (else
          (cons
            (car l)
            (omit-duplicates (cdr l))))))

(define (have-intersection? first second)
  (some?
    (lambda (f) (contains? f second))
    first))

(define (intersection . lists)
  (define (intersect-two first second)
    (cond ((null? first)
            '())
          ((contains? (car first) second)
            (cons
              (car first)
              (intersect-two
                (cdr first)
                second)))
          (else (intersect-two
                  (cdr first)
                  second))))
  (cond ((null? lists) '())
        ((equal? (length lists) 1) (car lists))
        (else (let ((first (car lists))
              (rest (cdr lists)))
          (apply
            intersection
            (map
              (lambda (second)
                (intersect-two first second))
              rest))))))

(define (union . lists)
  (omit-duplicates (apply append lists)))