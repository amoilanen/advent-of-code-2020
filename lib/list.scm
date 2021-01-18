(define (contains? el list)
  (not (eq? false (member el list))))

(define (split-list-by-list l splitters)
  (define (split l splitted-part already-splitted)
    (cond ((null? l)
            (cons (reverse splitted-part) already-splitted))
          ((contains? (car l) splitters)
            (split (cdr l) '() (cons (reverse splitted-part) already-splitted)))
          (else
            (split (cdr l) (cons (car l) splitted-part) already-splitted))))
  (reverse (split l '() '())))

(define (split-list-by l el)
  (split-list-by-list l (list el)))

(define (list-subtract l to-subtract)
  (define (loop remaining-elements acc)
    (cond ((null? remaining-elements) acc)
          ((contains?
             (car remaining-elements)
             to-subtract) 
           (loop
             (cdr remaining-elements)
             acc))
          (else
            (loop
              (cdr remaining-elements)
              (cons
                (car remaining-elements)
                acc)))))
  (reverse
    (loop l '())))

(define (list-flatten l)
  (apply append l))

(define (omit-empty l)
  (filter
    (lambda (p) (> (length p) 0))
    l))

(define (nth index list)
  (define (nth-loop n l)
    (cond ((null? l) (error "Index out of bounds" index list))
          ((eq? n 0) (car l))
          (else (nth-loop (- n 1) (cdr l)))))
  (if (< index 0) (error "Index out of bounds" index list)
    (nth-loop index list)))

(define (first-index-when p l)
  (define (loop p l index)
    (cond ((null? l) #f)
          ((p (car l)) index)
          (else (loop p (cdr l) (+ 1 index)))))
  (loop p l 0))

(define (find-indexes-where p l)
  (map
    (lambda (x)
      (car x))
    (filter
      (lambda (x)
        (p (cadr x)))
      (zip
        (range 0 (- (length l) 1))
        l))))

(define (first-index-of e l)
  (first-index-when
    (lambda (x)
      (equal? x e))
    l))

(define (range from to)
  (if (> from to) '()
    (cons from (range (+ 1 from) to))))

(define (count-of el l)
  (length
    (filter
      (lambda (x) (equal? x el))
      l)))

(define (take-at-most l n)
  (if (> n (length l))
    l
    (take l n)))

(define (drop-until p l)
  (cond ((null? l) '())
        ((p (car l)) l)
        (else (drop-until
                p
                (cdr l)))))

(define (drop-from-tail l num)
  (reverse
    (drop
      (reverse l)
      num)))

(define (sublist from to l)
  (let ((len (length l)))
    (drop-from-tail
      (drop l from)
      (- len 1 to))))

(define (every? predicate l)
  (cond ((null? l) #t)
        ((not (predicate (car l))) #f)
        (else (every? predicate (cdr l)))))

(define (some? predicate l)
  (not
    (every?
      (lambda (x)
        (not
          (predicate x)))
      l)))

(define (element-occurences-count el l)
  (length
    (filter
      (lambda (x) (eq? el x))
      l)))

(define (fill-till-length value len l)
  (define (loop remaining-len built-list)
    (if (<= remaining-len 0)
      built-list
      (loop (- remaining-len 1) (cons value built-list))))
  (loop (- len (length l)) l))