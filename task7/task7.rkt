#lang racket
(require  2htdp/image)
(require 2htdp/planetcute)
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

;===========drawing============

(define BOARD-SIZE 5)
(define queen-block character-horn-girl)
(define block-1 wood-block)
(define block-2 plain-block)


(define queen-maps (queens BOARD-SIZE))


(define (get-block-by-num num)
  (if (even? num)
      block-1
      block-2))

(define (queen-with-block block)
  (overlay/xy queen-block 0 40 block))

(define (get-col-stack position)
  (define (stack imgs)
  (cond
    [(empty? (rest imgs)) (first imgs)]
    [else (let ([drawing-image (first imgs)]
                [next-drawing-image (first (rest imgs))])
           (overlay/xy (stack (rest imgs))
                      0 (+ -80 (- (image-height next-drawing-image) (image-height drawing-image)))
                      drawing-image)
           )]))
  (stack (map (λ (cell-num) (let ([drawing-block (get-block-by-num (+ (cadr position) cell-num))])
                              (cond
                                ((equal? (add1 cell-num) (car position)) (queen-with-block drawing-block))
                                (else drawing-block)))) (range 0 BOARD-SIZE))))

(define (draw-map q-map)
  (scale 0.5 (apply beside/align (cons "bottom"
                          (map (λ (pos) (get-col-stack pos)) q-map))))
  )


(map (λ (q-map) (draw-map q-map)) queen-maps)
;(image-height (queen-with-block brown-block))
