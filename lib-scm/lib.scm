;;; Standard procedures.
;;; The rest are implementedly natively in Haskell, see src/Procedures.hs.

;;; 6.2.5  Numerical operations

(define (zero? z) (= z 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n)
  (not (= 0 (remainder n 2))))
(define (even? n)
  (= 0 (remainder n 2)))
(define (abs x)
  (if (< x 0) (- x) x))

;;; 6.3.1  Booleans

(define (not obj) (if obj #f #t))

;;; 6.3.2  Pairs and lists

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))

(define (caaar pair) (car (caar pair)))
(define (caadr pair) (car (cadr pair)))
(define (cadar pair) (car (cdar pair)))
(define (caddr pair) (car (cddr pair)))
(define (cdaar pair) (cdr (caar pair)))
(define (cdadr pair) (cdr (cadr pair)))
(define (cddar pair) (cdr (cdar pair)))
(define (cdddr pair) (cdr (cddr pair)))

(define (caaaar pair) (car (caaar pair)))
(define (caaadr pair) (car (caadr pair)))
(define (caadar pair) (car (cadar pair)))
(define (caaddr pair) (car (caddr pair)))
(define (cadaar pair) (car (cdaar pair)))
(define (cadadr pair) (car (cdadr pair)))
(define (caddar pair) (car (cddar pair)))
(define (cadddr pair) (car (cdddr pair)))
(define (cdaaar pair) (cdr (caaar pair)))
(define (cdaadr pair) (cdr (caadr pair)))
(define (cdadar pair) (cdr (cadar pair)))
(define (cdaddr pair) (cdr (caddr pair)))
(define (cddaar pair) (cdr (cdaar pair)))
(define (cddadr pair) (cdr (cdadr pair)))
(define (cdddar pair) (cdr (cddar pair)))
(define (cddddr pair) (cdr (cdddr pair)))

(define (list . lst) lst)

(define (length list)
  (define (go list acc)
    (if (null? list)
        acc
        (go (cdr list) (+ acc 1))))
  (go list 0))

(define (append . lists)
  (cond ((null? lists) '())
        ((null? (cdr lists)) (car lists))
        ((null? (car lists)) (apply append (cdr lists)))
        (else (cons (caar lists) (apply append (cdar lists) (cdr lists))))))

(define (reverse list)
  (define (go list acc)
    (if (null? list)
        acc
        (go (cdr list) (cons (car list) acc))))
  (go list '()))

(define (list-tail list k)
  (if (= k 0)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (car (list-tail list k)))

(define (member obj list)
  (define (go list)
    (cond ((null? list) #f)
          ((equal? obj (car list)) list)
          (else (go (cdr list)))))
  (go list))

(define (assoc obj alist)
  (define (go list)
    (cond ((null? list) #f)
          ((equal? obj (caar list)) (car list))
          (else (go (cdr list)))))
  (go alist))

;;; 6.3.5  Strings

(define (string . chars) (list->string chars))

;;; 6.3.6  Vectors

(define (vector . objs) (list->vector objs))

;;; 6.4  Control features

(define (map proc . lists)
  ;; The standard requires all lists to be the same length, but this implementation just stops when
  ;; the first list becomes empty.
  (define (map1 f list)
    (define (go list)
      (if (null? list)
          '()
          (cons (f (car list))
                (go (cdr list)))))
    (go list))
  (define (go lists)
    (if (null? (car lists))
        '()
        (cons (apply proc
                    (map1 car lists))
              (go (map1 cdr lists)))))
  (if (null? lists)
      '()
      (go lists)))

(define (for-each proc . lists)
  (apply map proc lists)
  (if #f #f))
