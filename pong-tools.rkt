#lang racket/base
(require racket/list)
(require racket/match)
(require "geometry.rkt")
(require "conf.rkt")
(require "renderer.rkt")

;x y are center coordinates not top - left
(define (get-rectangle-collision x y w h ball-x ball-y ball-r bsx bsy)
  (let* ([top (prtline (prtpoint (- x (/ w 2)) (- y (/ h 2)))(prtpoint (+ x (/ w 2)) (- y (/ h 2))))]
         [right (prtline (prtpoint (+ x (/ w 2)) (- y (/ h 2)))(prtpoint (+ x (/ w 2)) (+ y (/ h 2))))]
         [bottom (prtline (prtpoint (- x (/ w 2)) (+ y (/ h 2)))(prtpoint (+ x (/ w 2)) (+ y (/ h 2))))]
         [left (prtline (prtpoint (- x (/ w 2)) (- y (/ h 2)))(prtpoint (- x (/ w 2)) (+ y (/ h 2))))]

         [ball-center (prtpoint ball-x ball-y)]
         [ball-trajectory (prtline ball-center (struct-copy prtpoint ball-center [x (+ (prtpoint-x ball-center) bsx)] [y (+ (prtpoint-y ball-center) bsy)]))]
         [tophit (intersect-line-segments top ball-trajectory)]
         [righthit (intersect-line-segments right ball-trajectory)]
         [bottomhit (intersect-line-segments bottom ball-trajectory)]
         [lefthit (intersect-line-segments left ball-trajectory)])
    (list tophit righthit bottomhit lefthit)))

(define (get-nearest-brick ball-x ball-y ball-r coordinates)
  (argmin (lambda (x) (distance (prtpoint (list-ref x 0) (list-ref x 1)) (prtpoint ball-x ball-y) )) coordinates) )

(define (get-nearest-collision coordinates paddle-x paddle-y paddle-w paddle-h ball-x ball-y ball-r bsx bsy)
  (let* ([offset (* ball-r 0.707)]

         [nearest-brick-p1 (get-nearest-brick (- (+ ball-x bsx) offset) (- (+ ball-y bsy) offset) ball-r coordinates)]
         [nearest-brick-p2 (get-nearest-brick (- (+ ball-x bsx) offset) (+ (+ ball-y bsy) offset) ball-r coordinates)]
         [nearest-brick-p3 (get-nearest-brick (+ (+ ball-x bsx) offset) (- (+ ball-y bsy) offset) ball-r coordinates)]
         [nearest-brick-p4 (get-nearest-brick (+ (+ ball-x bsx) offset) (+ (+ ball-y bsy) offset) ball-r coordinates)]
         [nearest-brick-p5 (get-nearest-brick (+ ball-x bsx) (- (+ ball-y bsy) ball-r) ball-r coordinates)]
         [nearest-brick-p6 (get-nearest-brick (+ (+ ball-x bsx) ball-r) (+ ball-y bsy) ball-r coordinates)]
         [nearest-brick-p7 (get-nearest-brick (+ ball-x bsx) (+ (+ ball-y bsy) ball-r) ball-r coordinates)]
         [nearest-brick-p8 (get-nearest-brick (- (+ ball-x bsx) ball-r) (+ ball-y bsy) ball-r coordinates)]

         [brickhits (list (list 'brick (list-ref nearest-brick-p1 0) (list-ref nearest-brick-p1 1) (get-rectangle-collision (list-ref nearest-brick-p1 0) (list-ref nearest-brick-p1 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)))]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p2 0) (list-ref nearest-brick-p2 1) (get-rectangle-collision (list-ref nearest-brick-p2 0) (list-ref nearest-brick-p2 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p3 0) (list-ref nearest-brick-p3 1) (get-rectangle-collision (list-ref nearest-brick-p3 0) (list-ref nearest-brick-p3 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p4 0) (list-ref nearest-brick-p4 1) (get-rectangle-collision (list-ref nearest-brick-p4 0) (list-ref nearest-brick-p4 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p5 0) (list-ref nearest-brick-p5 1) (get-rectangle-collision (list-ref nearest-brick-p5 0) (list-ref nearest-brick-p5 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p6 0) (list-ref nearest-brick-p6 1) (get-rectangle-collision (list-ref nearest-brick-p6 0) (list-ref nearest-brick-p6 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p7 0) (list-ref nearest-brick-p7 1) (get-rectangle-collision (list-ref nearest-brick-p7 0) (list-ref nearest-brick-p7 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
         [brickhits (cons (list 'brick (list-ref nearest-brick-p8 0) (list-ref nearest-brick-p8 1) (get-rectangle-collision (list-ref nearest-brick-p8 0) (list-ref nearest-brick-p8 1) brick-w brick-h ball-x ball-y ball-r bsx bsy)) brickhits)]
            
         [paddle-hits (list 'paddle 0 0 (get-rectangle-collision paddle-x paddle-y paddle-w paddle-h ball-x ball-y ball-r bsx bsy))]
         [left-wall-hit (list 'left-wall 0 0 (get-rectangle-collision (* ball-r 2) (/ height 2) ball-r height ball-x ball-y ball-r bsx bsy))]
         [top-wall-hit (list 'top-wall 0 0 (get-rectangle-collision (/ width 2) (+ (* ball-r 3) 15) width ball-r ball-x ball-y ball-r bsx bsy))]
         [right-wall-hit (list 'right-wall 0 0 (get-rectangle-collision (- width (* ball-r 2)) (/ height 2) ball-r height ball-x ball-y ball-r bsx bsy))]
         [bottom-wall-hit (list 'bottom-wall 0 0 (get-rectangle-collision (/ width 2) (- height (* ball-r 2)) width ball-r ball-x ball-y ball-r bsx bsy))]
         [ball-point (prtpoint ball-x ball-y)]
         [hits (filter (lambda (x) (match x
                                     [(list _ _ _ (list #f #f #f #f)) #f]
                                     [_ #t])) (append brickhits (list paddle-hits left-wall-hit top-wall-hit right-wall-hit bottom-wall-hit)))]
         [hit (if (> (length hits) 1)
                  (argmin (lambda (x)
                            (let ([point (match (list-ref x 3)
                                           [(list (prtpoint x y) _ #f #f) (prtpoint x y)]
                                           [(list #f (prtpoint x y) _ #f) (prtpoint x y)]
                                           [(list #f #f (prtpoint x y) _) (prtpoint x y)]
                                           [(list _ #f #f (prtpoint x y)) (prtpoint x y)])])
                              (distance (prtpoint (+ ball-x bsx)(+ ball-y bsy)) point))) hits)
                  (if (> (length hits) 0)
                      (list-ref hits 0)
                      #f))]
         [nearest (match hit
                    [(list 'brick x y l) (match l
                                           [(list top #f #f #f) (list 'top x y)]
                                           [(list #f right #f #f) (list 'right x y)]
                                           [(list #f #f bottom #f) (list 'bottom x y)]
                                           [(list #f #f #f left) (list 'left x y)]
                                           [(list #f #f bottom left) (list 'bottom-left x y)]
                                           [(list #f right bottom #f) (list 'bottom-right x y)]
                                           [(list top right #f #f) (list 'top-right x y)]
                                           [(list top #f #f left) (list 'top-left x y)])]
                    [(list 'paddle _ _ l) 'paddle]
                    [(list 'left-wall _ _ l) 'left-wall]
                    [(list 'top-wall _ _ l) 'top-wall]
                    [(list 'right-wall _ _ l) 'right-wall]
                    [(list 'bottom-wall _ _ l) 'bottom-wall]
                    [#f #f]
                    )])
    ;res)) #f (list brickhits paddle-hits left-wall-hit top-wall-hit right-wall-hit bottom-wall-hit))])
    nearest))


(define (make-bricks coordinates)
  (map (lambda (x)  (brick (list-ref x 0) (list-ref x 1) 255)) coordinates))

(define (make-coordinates matrix)
  (map (lambda (x)  (list (+ (* (list-ref x 0) (+ brick-w gap-x)) (/ brick-w 2) offset-left ) (+ (* (list-ref x 1) (+ brick-h gap-y)) (/ brick-h 2) offset-top ))) matrix))

(provide (all-defined-out))