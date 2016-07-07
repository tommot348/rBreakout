#lang racket/base
(require racket/match)
(require lux)
(require lux/chaos/gui/key)
(require "pong-tools.rkt")
(require "conf.rkt")
(require "renderer.rkt")
(require "menu-word.rkt")
(require "game-over-word.rkt")
(require "sounds.rkt")

(struct pong (bricks matrix coordinates px ps bx by bsx bsy i lives score)
  #:methods gen:word
  [(define (word-fps w) 120.0)
   
   (define (word-label w ft) "Pong")
   
   (define (word-tick w)
     (let* ([collision (get-nearest-collision (pong-coordinates w) (pong-px w) paddle-y paddle-w paddle-h (pong-bx w) (pong-by w) ball-r (pong-bsx w) (pong-bsy w))]
            [cx (match collision
                  [#f 1]
                  [(list where _ _) (match where
                                      ['top 1]
                                      ['right -1]
                                      ['bottom 1]
                                      ['left -1]
                                      ['top-right -1]
                                      ['top-left -1]
                                      ['bottom-right -1]
                                      ['bottom-left -1])]
                  [where (match where
                           ['top-wall 1]
                           ['left-wall -1]
                           ['right-wall -1]
                           ['bottom-wall 0]
                           ['paddle 1])])]
            [cy (match collision
                  [#f 1]
                  [(list where _ _) (match where
                                      ['top -1]
                                      ['right 1]
                                      ['bottom -1]
                                      ['left 1]
                                      ['top-right -1]
                                      ['top-left -1]
                                      ['bottom-right -1]
                                      ['bottom-left -1])]
                  [where (begin (play-sounds (list whs))(match where
                           ['top-wall -1]
                           ['left-wall 1]
                           ['right-wall 1]
                           ['bottom-wall 0]
                           ['paddle (match (positive? (pong-bsy w))
                                      [#t -1]
                                      [#f 1])]))])]
            [score (match collision
                     [(list _ _ _) (begin (play-sounds (list bhs))(+ (pong-score w) 1))]
                     [_ (pong-score w)])]
            [lives (match collision
                     [(list _ _ _) (pong-lives w)]
                     [where (match where
                              ['bottom-wall (- (pong-lives w) 1)]
                              [_ (pong-lives w)])]
                     [_ (pong-score w)])]
            [coordinates (match collision
                      [(list _ x y) (remove (list x y) (pong-coordinates w))]
                      [_ (pong-coordinates w)])]
            [bricks (if (not (equal? coordinates (pong-coordinates w)))
                        (make-bricks coordinates)
                        (pong-bricks w))])
       (if (> lives 0)
           (struct-copy pong w
                    [i (+ (pong-i w) 1)]
                    [score score]
                    [lives lives]
                    [coordinates coordinates]
                    [bricks bricks]
                    [bsx (* cx (pong-bsx w))]
                    [bsy (* cy (pong-bsy w))]
                    [bx (if (= 0 cx) ball-start-x (+ (pong-bx w) (* cx (pong-bsx w))))]
                    [by (if (= 0 cy) ball-start-y (+ (pong-by w) (* cy (pong-bsy w))))]
                    [px (if (and (< (+ (pong-px w) (pong-ps  w)) (- width (/ paddle-w 2))) (> (+ (pong-px w) (pong-ps  w)) (/ paddle-w 2))) (+ (pong-px w) (pong-ps  w)) (pong-px w))])
           (let* ([coordinates (make-coordinates (matrix))]
                  [bricks (make-bricks coordinates)]
                  [game (pong bricks matrix coordinates (/ width 2.0) 0 ball-start-x ball-start-y 0.0 0.0 0 3 0)])
             (game-over score 0 game)))))
   
   (define (word-event w e)
     (cond
       [(eq? e 'close) #f]
       [(key-event? e) (cond
                         [(eq? (key-event-code e) 'left) (struct-copy pong w [ps -1.5])]
                         [(eq? (key-event-code e) 'right) (struct-copy pong w [ps 1.5])]
                         [(eq? (key-event-code e) 'up) (if (and (= (pong-bsx w) 0) (= (pong-bsy w) 0)) (struct-copy pong w [bsx 1] [bsy -1]) w)]
                         [(eq? (key-event-code e) 'escape) (menu 0 w 1)]
                         [else (struct-copy pong w [ps 0])])]       
       [else (struct-copy pong w [ps 0])]))
   
   (define (word-output w)
     (dc layers (list (pong-bricks w) (plasma (pong-i w)) cloud frame) (list  (paddle (pong-px w)) (ball (pong-bx w) (pong-by w)))))
   
   ])

(provide (struct-out pong))