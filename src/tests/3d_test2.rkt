#lang racket
 
(require pict3d
         pict3d/universe)
 
;; (current-material (material #:ambient 0.01
;;                             #:diffuse 0.39
;;                             #:specular 0.6
;;                             #:roughness 0.2))
 
(define lights+camera
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos 4 4 0) origin))))

;; (pos x y z)
;; (with-color (rgba "color") (object position scale))
;; (combine object1 object2 ... objectN)

(define (on-draw s n t) 
    (combine 
;;        (rotate-z lights+camera (/ t 30)) 
        lights+camera
        (rotate-z 
          (combine
            (with-color (rgba "pink") (cube (pos 0 0 1) 1/2))
            (with-color (rgba "orange") (cube (pos 0 0 -1) 1/2))
            (with-color (rgba "white") (cube (pos 0 1 0) 1/2))
            (with-color (rgba "yellow") (cube (pos 0 -1 0) 1/2))
            ;(rotate-x
              (with-color (rgba "blue") (cube (pos 1 0 0) 1/2))
              ;(/ t 30))
            (with-color (rgba "green") (cube (pos -1 0 0) 1/2)))
            ;(face1 -1 0 0 t)
         45 
        )
        ))
        

(define (face1 x y z t)
  (rotate-x 
    (with-color (rgba "violet") (cube (pos x y z) 1/2)) (/ t 5)))
 
(big-bang3d 0 #:on-draw on-draw)