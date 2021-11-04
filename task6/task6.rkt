#lang racket
(require racket/trace)

(define (safe? k positions)
  (let ([target-row (car (car positions))]
        [target-col (cadr (car positions))])
    (empty? (filter (λ (position)
                      (or
                       (equal? target-row (car position))
                       (equal? (abs (- (car position) target-row))
                               (abs (- (cadr position) target-col)))))
                    (cdr positions)))))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (λ (positions) (safe? k positions))
                (append-map
                 (λ (rest-of-queens)
                   (map (λ (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (range 1 (+ board-size 1))))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

;(trace safe?)

(queens 8)