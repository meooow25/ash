;;; Quicksort-like algorithm, not in-place.
(define (sort lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (else (let* ((x (car lst))
                     (acctf (partition (lambda (y) (< y x)) (cdr lst))))
                    (append (sort (car acctf))
                            (list x)
                            (sort (cdr acctf)))))))

(define (partition f lst)
  (define (go lst acct accf)
    (cond ((null? lst) (cons acct accf))
          ((f (car lst)) (go (cdr lst)
                             (cons (car lst) acct)
                             accf))
          (else (go (cdr lst)
                    acct
                    (cons (car lst) accf)))))
  (go lst '() '()))

;;; Random list with a simple LCG.
;;; https://pubs.opengroup.org/onlinepubs/9699919799/functions/rand.html
(define (rand-list seed n)
  (if (= n 0)
      '()
      (let ((seed1 (remainder (+ 12345 (* 1103515245 seed)) #x100000000)))
           (cons (remainder (quotient seed1 65536) 32768)
                 (rand-list seed1 (- n 1))))))

(sort (rand-list 1 20000))
