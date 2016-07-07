#lang racket/base
(require racket/match)
(require lux)
(require lux/chaos/gui/key)
(require "rBreakout-tools.rkt")
(require "conf.rkt")
(require "renderer.rkt")
(require "menu-word.rkt")
(require "game-over-word.rkt")
(require "sounds.rkt")

(struct rBreakout (bricks matrix coordinates px ps bx by bsx bsy i lives score)
  #:methods gen:word
  [(define (word-fps w) 120.0)
   
   (define (word-label w ft) "rBreakout")
   
   (define (word-tick w)
     (let* ([collision (get-nearest-collision (rBreakout-coordinates w) (rBreakout-px w) paddle-y paddle-w paddle-h (rBreakout-bx w) (rBreakout-by w) ball-r (rBreakout-bsx w) (rBreakout-bsy w))]
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
                           ['paddle (match (positive? (rBreakout-bsy w))
                                      [#t -1]
                                      [#f 1])]))])]
            [score (match collision
                     [(list _ _ _) (begin (play-sounds (list bhs))(+ (rBreakout-score w) 1))]
                     [_ (rBreakout-score w)])]
            [lives (match collision
                     [(list _ _ _) (rBreakout-lives w)]
                     [where (match where
                              ['bottom-wall (- (rBreakout-lives w) 1)]
                              [_ (rBreakout-lives w)])]
                     [_ (rBreakout-score w)])]
            [coordinates (match collision
                      [(list _ x y) (remove (list x y) (rBreakout-coordinates w))]
                      [_ (rBreakout-coordinates w)])]
            [bricks (if (not (equal? coordinates (rBreakout-coordinates w)))
                        (make-bricks coordinates)
                        (rBreakout-bricks w))])
       (if (> lives 0)
           (struct-copy rBreakout w
                    [i (+ (rBreakout-i w) 1)]
                    [score score]
                    [lives lives]
                    [coordinates coordinates]
                    [bricks bricks]
                    [bsx (* cx (rBreakout-bsx w))]
                    [bsy (* cy (rBreakout-bsy w))]
                    [bx (if (= 0 cx) ball-start-x (+ (rBreakout-bx w) (* cx (rBreakout-bsx w))))]
                    [by (if (= 0 cy) ball-start-y (+ (rBreakout-by w) (* cy (rBreakout-bsy w))))]
                    [px (if (and (< (+ (rBreakout-px w) (rBreakout-ps  w)) (- width (/ paddle-w 2))) (> (+ (rBreakout-px w) (rBreakout-ps  w)) (/ paddle-w 2))) (+ (rBreakout-px w) (rBreakout-ps  w)) (rBreakout-px w))])
           (let* ([coordinates (make-coordinates (matrix))]
                  [bricks (make-bricks coordinates)]
                  [game (rBreakout bricks matrix coordinates (/ width 2.0) 0 ball-start-x ball-start-y 0.0 0.0 0 3 0)])
             (game-over score 0 game)))))
   
   (define (word-event w e)
     (cond
       [(eq? e 'close) #f]
       [(key-event? e) (cond
                         [(eq? (key-event-code e) 'left) (struct-copy rBreakout w [ps -1.5])]
                         [(eq? (key-event-code e) 'right) (struct-copy rBreakout w [ps 1.5])]
                         [(eq? (key-event-code e) 'up) (if (and (= (rBreakout-bsx w) 0) (= (rBreakout-bsy w) 0)) (struct-copy rBreakout w [bsx 1] [bsy -1]) w)]
                         [(eq? (key-event-code e) 'escape) (menu 0 w 1)]
                         [else (struct-copy rBreakout w [ps 0])])]       
       [else (struct-copy rBreakout w [ps 0])]))
   
   (define (word-output w)
     (dc layers (list (rBreakout-bricks w) (plasma (rBreakout-i w)) cloud frame) (list  (paddle (rBreakout-px w)) (ball (rBreakout-bx w) (rBreakout-by w)))))
   
   ])

(provide (struct-out rBreakout))
