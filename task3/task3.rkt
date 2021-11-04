#lang racket
(require racket/trace)

(define (repeated foo count)
  (define (repeated-helper fres count)
    (if (<= count 0)
      identity
      (repeated-helper (compose fres foo) (- count 1)))
    )
  (trace repeated-helper)
  (repeated-helper foo count))

(define (repeated-lambda foo count)
  (define (repeated-helper fres count)
    (if (<= count 0)
      identity
      (repeated-helper (λ (x) (fres (foo x))) (- count 1)))
    )
  (trace repeated-helper)
  (repeated-helper foo count))

((repeated sqr 2) 2)
