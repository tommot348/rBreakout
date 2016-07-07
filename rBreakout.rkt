#lang racket/base

(require lux)
(require lux/chaos/gui)
;(require lux/chaos/3s)
;(require lux/chaos/pair)

(require "conf.rkt")
(require "rBreakout-tools.rkt")
(require "renderer.rkt")
(require "game-word.rkt")
(require "menu-word.rkt")



;(gl-filter-mode 'crt)

(define coordinates (make-coordinates (matrix)))
(define bricks (make-bricks coordinates))
(define game (rBreakout bricks matrix coordinates (/ width 2.0) 0 ball-start-x ball-start-y 0.0 0.0 0 3 0))
(call-with-chaos (make-gui #:mode gui-mode) (lambda() (fiat-lux (menu 0 game 0))) )