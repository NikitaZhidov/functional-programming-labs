#lang racket
(require racket/trace)

;helpers
(define (inc x)
  (+ x 1))
;helpers


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact n)
  (product identity 1 add1 n))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (accumulate-helper acc a)
    (if (> a b)
        acc
        (accumulate-helper (combiner acc (term a)) (next a))))
  (trace accumulate-helper)
  (accumulate-helper null-value a))

(trace sum)
(trace product)
(trace accumulate)

;(fact 6)

;(product (λ (x) (sqr x)) 1 inc 5)
;(accumulate * 1 (λ (x) (sqr x)) 1 inc 5)
;(accumulate-iter * 1 (λ (x) (sqr x)) 1 inc 5)


;(sum identity 1 inc 5)
;(accumulate + 0 identity 1 inc 5)
;(accumulate-iter + 0 identity 1 inc 5)



(define (repeated foo count)
  (accumulate-iter compose identity (const foo) 1 add1 count))

((repeated sqr 2) 5)