;; ########## Variables del programa ##########

;; ##### Variables de animación #####
;; Variable que define el tiempo que transcurre entre los pasos
(define step-wait-time 3000)
;; Variable que define la duración de la animación de un paso
(define animation-duration 1500)

;; ##### Variables de configuración #####
;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);

;; Variable que define el incremento de coordenadas entre el cubo de color y el cubo base
(define coord-step-color 0.2);

;; Variable que define el tamaño por defecto de los cubo base
(define base-cube-size 0.96)
;; Variable que define el tamaño por defecto de los cubos de color
(define color-cube-size 4/5)

;; ##### Variables de estado #####
;; Variable que indica si se debe actualizar el estado gráfico del cubo.
(define update_state #t)
;; Variable que define si se está rotando.
(define rotating #f)
;; Variable que define si se está rotando una fila o columna.
(define rotating-col #f)
;; Variable que define el índice de rotación.
(define rotating-index 0)
;; Variable que almacena el estado actual del cubo
(define current-state '())

;; ##### Variables principales #####
;; Variable para almacenar el tamaño del cubo.
(define cube-size 2)
;; Variable para almacenar el estado interno del cubo.
(define cube-internal-state '())
;; Variable para almacenar los movimientos a realizar.
(define cube-steps '())


(define cube_prueba
    '(
        (#F (("B1" "B1" "B1")
             ("Y2" "Y2" "Y2")
             ("B3" "B3" "B3")))
        (#T (("B1" "B1" "B1")
             ("W2" "W2" "W2")
             ("W3" "W3" "W3")))
        (#F (("G1" "G1" "G1")
             ("W2" "W2" "W2")
             ("G3" "G3" "G3")))
        (#F (("Y1" "Y1" "Y1")
             ("G2" "G2" "G2")
             ("Y3" "Y3" "Y3")))
        (#F (("P1" "P1" "P1")
             ("P2" "P2" "P2")
             ("P3" "P3" "P3")))
        (#F (("O1" "O1" "O1")
             ("O2" "O2" "O2")
             ("O3" "O3" "O3")))
    )
)
