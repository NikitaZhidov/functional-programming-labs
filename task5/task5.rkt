#lang racket
(require racket/trace)

(define (subsets set)  
  (if (empty? set)
      '(())
      (let ((cur-subsets (subsets (cdr set))))
        (append cur-subsets (map (λ (subset) (cons (car set) subset)) cur-subsets)))))

(trace subsets)
(subsets '(1 2 3))
 