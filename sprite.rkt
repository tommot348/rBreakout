#lang racket/base
(require (prefix-in image: 2htdp/image))
(require mode-lambda)
(require mode-lambda/text)


(require "conf.rkt")

(define db (make-sprite-db))
;(add-palette! db 'default (color->palette (argb 255 0 0 255)))
(add-sprite!/value db 'brick (image:overlay (image:rectangle brick-w brick-h 'outline 'white) (image:rectangle brick-w brick-h 'solid 'blue) ))
(add-sprite!/value db 'paddle (image:rectangle paddle-w paddle-h 'solid 'brown))
(add-sprite!/value db 'ball (image:circle ball-r 'solid 'green))
(add-sprite!/file db 'background "sprites-raw/bg.png")
(add-sprite!/file db 'plasma "sprites-raw/plasma.png")
(add-sprite!/file db 'cloud "sprites-raw/cloud.png")
(add-sprite!/file db 'menu "sprites-raw/menu.png")
(add-sprite!/file db 'selected "sprites-raw/selected.png")
(add-sprite!/file db 'game-over "sprites-raw/game-over.png")

(define font (load-font! db #:size 16.0
                         #:face "Px437 IBM ISO9"
                          #:smoothing 'smoothed
                          #:weight 'bold
                          #:family 'modern))

(define cdb (compile-sprite-db db))

(save-csd! cdb "sprites")