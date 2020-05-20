;; ##### Variables de configuración del modo interactivo #####

;; Variable que almacena el ángulo de rotación en el eje X.
(define cube-rotation-x 0.0)
;; Variable que almacena el ángulo de rotación en el eje Y.
(define cube-rotation-y 0.0)
;; Variable que almacena el ángulo de rotación en el eje Z.
(define cube-rotation-z 0.0) 

;; Variable que indica el tipo de movimiento a realizar: #F => Filas, #T => Columnas.
(define movement-type-selected #F)
;; Variable que indica que fila o columna se va a rotar: De 0 a N.
(define movement-index 0)
;; Variable que indica la dirección del movimiento a realizar: #T => Izquierda-Arriba, #F => Derecha-Abajo.
(define movement-direction #T)