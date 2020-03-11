#lang racket
(require pic3d
         pic3d/universe)

(define example sphere origin 1/2)

(define (on-draw s n t)
  (combine (rotate-z (rotate-y (rotate-x (cube origin 1/2)
                                         (/ t 11))
                               (/ t 13))
                     (/ t 17))
           lights+camera))

(big-bang3d 0 #:on-draw on-draw)