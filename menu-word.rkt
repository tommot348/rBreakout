#lang racket/base
(require racket/match)
(require lux)
(require lux/chaos/gui/key)
(require "renderer.rkt")
(require "conf.rkt")
(require "sounds.rkt")

(struct menu (state game i)
  #:methods gen:word
  [
   (define (word-label w ft) "rBreakout Menu")
   (define (word-tick w) (if (= 0 (menu-i w)) (begin
                                                (play-bg)
                                                (struct-copy menu w [i 1])) w))
   (define (word-output w)
     (let ([ypos (if (= 0 (menu-state w)) ypos-game ypos-exit)])
     (dc layers (list menu-screen) (list (selected ypos)))))
   (define (word-event w e) (cond
                              [(eq? #f e) (stop-playback) #f]
                              [(eq? 'close e) (stop-playback) #f]
                              [(key-event? e) (match (key-event-code e)
                                                ['escape (stop-playback) #f]
                                                ['up (play-sounds (list selso))(struct-copy menu w [state (modulo (+ (menu-state w) 1) 2)])]
                                                ['down (play-sounds (list selso))(struct-copy menu w [state (modulo (+ (menu-state w) 1) 2)])]
                                                [#\return (if (= 0 (menu-state w))
                                                              (menu-game w)
                                                              (begin
                                                                (stop-playback)
                                                                #f))]
                                                [_ w])]
                              [else w]))])

(provide (struct-out menu))