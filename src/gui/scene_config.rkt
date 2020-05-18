;; ########## Variables del programa ##########

;; ##### Variables de animación #####
;; Variable que define el tiempo que transcurre entre los pasos
(define step-default-time 250.0)
(define step-wait-time step-default-time)
;; Variable que define la duración de la animación de un paso
(define animation-default-time 500.0)
(define animation-duration animation-default-time)

(define delta-time 0.0)
(define prev-time 0.0)

;; ##### Variables de configuración #####
;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);
;; Variable que define el incremento de coordenadas entre el cubo de color y el cubo base
(define coord-step-color 0.30);
;; Variable que define el tamaño por defecto de los cubo base
(define base-cube-size 0.96)
;; Variable que define el tamaño por defecto de los cubos de color
(define color-cube-size 0.7)

;; ##### Variables de estado #####
;; Variable que indica si se debe actualizar el estado gráfico del cubo.
(define update_state #t)
;; Variable que indica el estado lógico del programa
(define logic-state 0)
;; Variable que define el ángulo actual de la rotación.
(define rotation-angle 0)
;; Variable que almacena el estado actual del cubo.
(define current-state '())
;; Variable que almacena el objeto pict3d del cubo.
(define current-obj-state empty-pict3d)
;; Variable que indica si el programa terminó de realizar los pasos.
(define program-done #f)
;; Variable que se utiliza para la rotación de la cámara al terminar de realizar los pasos.
(define camera-rotation 0.0)

;; ##### Variables principales #####
;; Variable para almacenar el tamaño del cubo.
(define cube-size 3)
;; Variable para almacenar el estado interno del cubo.
(define cube-internal-state '())
;; Variable para almacenar los movimientos a realizar.
(define cube-steps '("C0B" "F1I" "C2A" "C0B" "F2D"))
