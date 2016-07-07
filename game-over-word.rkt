#lang racket/base
(require racket/match)
(require lux)
(require lux/chaos/gui/key)
(require "pong-tools.rkt")
(require "conf.rkt")
(require "renderer.rkt")

(struct game-over (score state game)
  #:methods gen:word [
                      (define (word-label w ft) "Pong")
                      (define (word-event w e) (cond
                                                   [(key-event? e) (match (key-event-code e)
                                                                     ['up (struct-copy game-over w [state (modulo (+ (game-over-state w) 1) 2)])]
                                                                     ['down (struct-copy game-over w [state (modulo (+ (game-over-state w) 1) 2)])]
                                                                     [#\return (if (= (game-over-state w) 0) (game-over-game w) #f)]
                                                                     ['escape  #f]
                                                                     [_ w])]
                                                   [else w]))
                      (define (word-output w) 
                        (let ([ypos (if (= 0 (game-over-state w)) ypos-game ypos-exit)])
                          (dc layers (list game-over-screen) (list (selected ypos)))))])

(provide (struct-out game-over))
