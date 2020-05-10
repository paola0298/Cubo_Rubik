

;; Cámara de la escena
;(define lights-camera
;    (combine 
;        (light (pos 20 0 0) (emitted "white" 50))
;        (light (pos 0 20 0) (emitted "white" 50))
;        (light (pos 0 0 20) (emitted "white" 50))
;        (basis 'camera (point-at (pos 14 14 14) origin))))

;; Ejes de coordenadas para referencia
(define coords
    (combine
        (with-color (rgba "red") (arrow origin (pos 0 0 10)))
        (with-color (rgba "green") (arrow origin (pos 0 10 0)))
        (with-color (rgba "blue") (arrow origin (pos 10 0 0)))
    ))