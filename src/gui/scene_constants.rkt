(current-material 
    (material 
        #:ambient 0.5
        #:diffuse 0.4
        #:specular 0.1
        #:roughness 0.5))

;; Cámara de la escena
(define lights-camera
    (combine 
        (light (pos 5 0 0) (emitted "white" 25))
        (light (pos 0 5 0) (emitted "white" 25))
        (light (pos 0 0 5) (emitted "white" 25))
        (basis 'camera (point-at (pos 5 5 4) origin))))

;; Ejes de coordenadas para referencia
(define coords
    (combine
        (with-color (rgba "red") (arrow origin (pos 0 0 4)))
        (with-color (rgba "green") (arrow origin (pos 0 4 0)))
        (with-color (rgba "blue") (arrow origin (pos 4 0 0)))
    ))