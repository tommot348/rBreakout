#lang racket/base
(require racket/match)
(require lux)
(require lux/chaos/gui/key)
;(require "rBreakout-tools.rkt")
(require "conf.rkt")
(require "renderer.rkt")
(require "sounds.rkt")

(struct game-over (score state game)
  #:methods gen:word [
                      (define (word-label w ft) "rBreakout Game over")
                      (define (word-event w e) (cond
                                                 [(eq? #f e) (stop-playback) #f]
                                                 [(eq? 'close e) (stop-playback) #f]
                                                 [(key-event? e) (match (key-event-code e)
                                                                     ['up (begin (play-sounds (list selso)) (struct-copy game-over w [state (modulo (+ (game-over-state w) 1) 2)]))]
                                                                     ['down (begin (play-sounds (list selso)) (struct-copy game-over w [state (modulo (+ (game-over-state w) 1) 2)]))]
                                                                     [#\return (if (= (game-over-state w) 0) (game-over-game w) (begin (stop-playback) #f))]
                                                                     ['escape  (begin (stop-playback) #f)]
                                                                     [_ w])]
                                                   [else w]))
                      (define (word-output w) 
                        (let ([ypos (if (= 0 (game-over-state w)) ypos-game ypos-exit)])
                          (dc layers (list game-over-screen) (list (selected ypos)))))])

(provide (struct-out game-over))
