;; ########## Variables del programa ##########

;; Variable que define el tiempo que transcurre entre los pasos
(define step-wait-time 3000)

;; Variable que define la duración de la animación de un paso
(define animation-duration 1500)

;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);

;; Variable que define el tamaño por defecto de los cubo base
(define base-cube-size 0.96)

;; Variable que define el tamaño por defecto de los cubos de color
(define color-cube-size 4/5)

;; Variable que almacena el estado actual del cubo
(define current-state empty-pict3d)

;; Variable que indica si se debe actualizar el estado gráfico del cubo.
(define update_state #t)