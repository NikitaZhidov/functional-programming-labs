#lang racket
(require racket/trace)

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(trace pow)

(define (pow-iter x n)
  (define (pow-helper c n)
    (if (= n 0)
        c
        (pow-helper (* c x) (- n 1))))
  (trace pow-helper)
  (pow-helper 1 n))

;(pow 2 6)
(pow-iter 2 8)
