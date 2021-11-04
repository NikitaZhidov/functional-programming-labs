#lang racket
(require racket/trace)

(define (append-two a b)
  (if (empty? a)
      b
      (cons (car a) (append-two (cdr a) b))))

(define (append-lists lists)
  (if (empty? lists)
      '()
      (append-two (car lists) (append-lists (cdr lists)))))

(define append-multiple
  (lambda lists
    (append-lists lists)))

;====fold=====

(define (append-two-fold a b)
  (foldr cons b a))

(define (append-multiple-fold . lists)
  (foldr append-two-fold '() lists))
;====fold=====

(trace append-lists)
(trace append-multiple)
(trace append-two-fold)
(trace append-multiple-fold)

;(append-two-fold '(a b) '(c d e))
(append-multiple-fold '(a b) '(c d e) '(e f g))
;(append-multiple '(a f k) '(b c d e) '(3 4 5))