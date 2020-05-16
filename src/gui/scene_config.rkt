;; ########## Variables del programa ##########

;; ##### Variables de animación #####
;; Variable que define el tiempo que transcurre entre los pasos
(define step-wait-time 3000)
;; Variable que define la duración de la animación de un paso
(define animation-duration 1500)

;; ##### Variables de configuración #####
;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);
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
