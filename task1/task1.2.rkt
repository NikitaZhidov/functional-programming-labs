#lang racket
(require racket/trace)

(define (pow x n)
  (cond ((= n 0) 1)
        ((= (modulo n 2) 1) (* x (pow x (- n 1))))
        (else (sqr (pow x (/ n 2))))
        )
  )
(trace pow)

(define (pow-iter x n)
  (define (pow-helper c x n)
    (cond ((= n 0) c)
        ((= (modulo n 2) 1) (pow-helper (* c x) x (- n 1)))
        (else (pow-helper c (sqr x) (/ n 2)))
        )
    )

  (trace pow-helper)
  (pow-helper 1 x n)
  )

;(pow 2 6)
(pow-iter 2 2)