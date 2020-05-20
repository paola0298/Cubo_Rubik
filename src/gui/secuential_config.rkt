;; ##### Variables de configuración del modo secuencial #####

;; Variable que define el tiempo que se espera entre paso y paso.
(define step-default-time 250.0)
(define step-wait-time step-default-time)
;; Variable que define el tiempo que se dura realizando la animación de rotacioń. NO IMPLEMENTADO.
(define animation-default-time 500.0)
(define animation-duration animation-default-time)

;; Variable que indica el tiempo transcurrido entre la última llamada a la función y la llamada actual.
(define delta-time 0.0)
;; Variable que indica el tiempo anterior del programa.
(define prev-time 0.0)

;; ##### Variables de estado #####
;; Variable que indica el estado lógico del programa
(define logic-state 0)
;; Variable que define el ángulo actual de la rotación. NO IMPLEMENTADO.
(define rotation-angle 0)
;; Variable que indica si el programa terminó de ejecutar los pasos dados.
(define program-done #f)
;; Variable que indica el ángulo de rotación de la cámara.
(define camera-rotation 0.0)

;; Variable que indica los pasos a aplicar al cubo.
(define cube-steps '())
