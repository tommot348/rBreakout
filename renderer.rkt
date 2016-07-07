#lang racket/base
(require mode-lambda)
(require mode-lambda/backend/gl)
(require "conf.rkt")

(define cdb (load-csd "sprites"))

(define dc (stage-draw/dc cdb width height))

(define layers
  (vector
   (layer (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) #:mx 0.99 #:my 0.99) ;cloud
   (layer (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) #:mode7 3.0 #:fov 15.0 #:horizon 200.0 #:mx 0.99 #:my 0.99) ;rotating plasma
   (layer (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) #:mx 0.99 #:my 0.99) ;game
   (layer (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) #:mx 0.9999 #:my 0.9999) ;frame
   #f #f #f #f))

(define cloud (sprite (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) (sprite-idx cdb 'cloud) #:layer 0))
(define (plasma i) (sprite (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) (sprite-idx cdb 'plasma) #:theta (exact->inexact (* 0.01 i)) #:layer 1))
(define frame (sprite (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) (sprite-idx cdb 'background) #:layer 3))
(define menu-screen (sprite  (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) (sprite-idx cdb 'menu) #:layer 0))
(define game-over-screen (sprite  (exact->inexact (/ width 2)) (exact->inexact (/ height 2)) (sprite-idx cdb 'game-over) #:layer 0))
(define (selected y) (sprite  (exact->inexact (/ width 2)) (exact->inexact y) (sprite-idx cdb 'selected) #:layer 2))

(define (brick x y a)
  (sprite (exact->inexact x) (exact->inexact y) (sprite-idx cdb 'brick) #:a (exact->inexact a) #:layer 2))

(define (paddle x)
  (sprite x paddle-y (sprite-idx cdb 'paddle) #:layer 2))

(define (ball x y)
  (sprite x y (sprite-idx cdb 'ball) #:layer 2))

(provide dc cloud plasma frame menu-screen game-over-screen selected brick paddle ball layers gui-mode)