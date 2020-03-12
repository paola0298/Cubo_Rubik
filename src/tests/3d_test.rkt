#lang racket
 
(require pict3d
         pict3d/universe)
 
(current-material (material #:ambient 0.1
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.2))
 
(define lights+camera
  (combine (light (pos 0 1 2) (emitted "white" 7))
           (light (pos 0 -1 -2) (emitted "white" 7))
           (basis 'camera (point-at (pos 1.5 1.5 1.5) origin))))
 
(define (on-draw s n t) 
    (combine 
        (rotate-z 
            (combine 
                (with-color (rgba "gray") (cube (pos 0 0 0) 1/2))
                (with-color (rgba "yellow") (cube (pos 0.12 0 0) 0.4))
                (with-color (rgba "green") (cube (pos 0 0.12 0) 0.4))
                (with-color (rgba "pink") (cube (pos 0 0 0.12) 0.4))
            ) 
            (/ t 30)) 
        lights+camera

    ))


(big-bang3d 0 #:on-draw on-draw)
