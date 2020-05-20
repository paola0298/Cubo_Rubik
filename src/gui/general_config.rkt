;; ##### Variables de configuración general #####
;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);
;; Variable que define el incremento de coordenadas entre el cubo de color y el cubo base
(define coord-step-color 0.30);
;; Variable que define el tamaño por defecto de los cubo base
(define base-cube-size 0.96)
;; Variable que define el tamaño por defecto de los cubos de color
(define color-cube-size 0.7)

;; Variable que indica si se debe actualizar el estado gráfico del cubo.
(define update_state #t)
;; Variable que almacena el estado actual del cubo.
(define current-state '())

;; ##### Variables principales #####
;; Variable para almacenar el tamaño del cubo.
(define cube-size 3)
;; Variable para almacenar el estado interno del cubo.
(define cube-internal-state '())

;; Configuración del color de fondo.
(current-pict3d-background (rgba "gray"))
