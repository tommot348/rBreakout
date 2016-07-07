#lang racket
(struct prtpoint (x y))
(struct prtline (p1 p2))

(define (distance p1 p2)
  (if (and (prtpoint? p2) (prtpoint? p2))
      (let* ([x1 (prtpoint-x p1)]
             [y1 (prtpoint-y p1)]
             [x2 (prtpoint-x p2)]
             [y2 (prtpoint-y p2)]
             [distx (abs (- x1 x2))]
             [disty (abs (- y1 y2))])
        ;(sqrt (+ (expt distx 2.0)(expt disty 2.0))) )
        (sqrt (+ (* distx distx)(* disty disty))) )
      (error "Arguments must be [prtpoint prtpoint]" -1)))

(define (is-on-segment? point line)
  (if (and (prtpoint? point) (prtline? line))
      (= (+ (distance (prtline-p1 line) point) (distance (prtline-p2 line) point))
         (distance (prtline-p1 line)(prtline-p2 line)) ) ;distance line-p1 -> point + distance line-p2 -> point == distance line-p1  -> line-p2
      (error "Arguments must be [prtpoint prtline]" -1)))

(define (intersect-lines l1 l2)
  (if (and (prtline? l1) (prtline? l2))
      ;calculate determinants
      (let* ([x1 (prtpoint-x (prtline-p1 l1))]
             [y1 (prtpoint-y (prtline-p1 l1))]
             [x2 (prtpoint-x (prtline-p2 l1))]
             [y2 (prtpoint-y (prtline-p2 l1))]
             [x3 (prtpoint-x (prtline-p1 l2))]
             [y3 (prtpoint-y (prtline-p1 l2))]
             [x4 (prtpoint-x (prtline-p2 l2))]
             [y4 (prtpoint-y (prtline-p2 l2))]
             [denominator (- (* (- x1 x2)(- y3 y4))(* (- y1 y2)(- x3 x4)))])
        (if (not (= denominator 0))
            (let 
                ([px (/
                      (- (* (- (* x1 y2) (* y1 x2))(- x3 x4))(*  (- x1 x2) (- (* x3 y4)(* y3 x4))))
                      denominator)]
                 [py (/
                      (- (* (- (* x1 y2)(* y1 x2))(- y3 y4)) (* (- y1 y2) (- (* x3 y4)(* y3 x4))))
                      denominator)]
                 )
              (prtpoint px py))
            #f))
      ;nope
      (error "Arguments must be prtlines" -1)))

(define (intersect-line-segments l1 l2)
  (if (and (prtline? l1) (prtline? l2))
      (let ([point (intersect-lines l1 l2)])
        ;is point on both line-segments?
        (if (and (prtpoint? point) (is-on-segment? point l1) (is-on-segment? point l2))
            point
            #f))
      
      ;nope
      (error "Arguments must be prtlines" -1)))

(provide (struct-out prtpoint) (struct-out prtline) distance is-on-segment? intersect-lines intersect-line-segments)
