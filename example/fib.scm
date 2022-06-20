;;; Classic dumb way to find the nth Fibonacci number.
(define (fib n)
    (if (<= n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))
(display (fib 30))
(newline)
