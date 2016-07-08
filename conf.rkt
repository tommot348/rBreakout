#lang racket/base
(require racket/list)
(define width 640)
(define height 480)

(define ball-r 5)
(define paddle-w 20)
(define paddle-h 10)
(define brick-w 30)
(define brick-h 10)

(define bricks-cx 19)
(define bricks-cy 10)

(define gap-x 2)
(define gap-y 2)

(define offset-left 10)
(define offset-bottom 10)
(define offset-right 10)
(define offset-top  60)

(define ypos-game 252)
(define ypos-exit 334)

(define ball-start-x 300.0)
(define ball-start-y 300.0)

(define paddle-y (- height 30.0))

(define (make-matrix)
  (filter (lambda (x) (= 255 (list-ref x 2)))
          (apply append
                 (map (lambda (y)
                        (map (lambda (x)
                               (if (< (random 10) 5)
                                   (list x y 255)
                                   (list x y 0))) (range bricks-cx))) (range bricks-cy)))))

(provide (all-defined-out))