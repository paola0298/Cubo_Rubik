;; Función para obtener el tiempo delta.
;; @param t1: Tiempo anterior.
;; @param t2: Tiempo actual.
(define (get-delta t1 t2)
    (abs (- t2 t1)))

;; Función para verificar el estado lógico del programa y modificarlo según sea necesario
;; @param delta: Tiempo desde el inicio del programa.
(define (check-state delta)
    (set! delta-time (get-delta prev-time delta))
    (set! prev-time delta)
    (cond
        ((equal? logic-state 0) ; do-step
            (cond 
                ((null? cube-steps)
                    (set! program-done #T)
                    current-state)
                (else 
                    (set! logic-state 1)
                    (set! cube-internal-state 
                    (do-step (car cube-steps) cube-size cube-internal-state))
                    (set! cube-steps (cdr cube-steps))
                    current-state)
            ))
        ((equal? logic-state 1) ; rotating => actualizar cubo |NO IMPLEMENTADO|
            (cond
                ((> 0 animation-duration)
                    (set! animation-duration animation-default-time)
                    (set! logic-state 2)
                    current-state)
                (else
                    (set! animation-duration (- animation-duration delta-time))
                    ;Seleccionar fila o columna de rotación y rotar por la variable de ángulo.
                    current-state)
            )
            current-state)
        ((equal? logic-state 2) ; step-done
            (set! logic-state 3)
            (set! rotation-angle 0)
            (set! update_state #T)
            (update_cube current-state cube-size delta)
            current-state)
        ((equal? logic-state 3) ; delay-time
            (cond
                ((>= 0 step-wait-time)
                    (set! step-wait-time step-default-time)
                    (set! logic-state 0)
                    current-state)
                (else
                    (set! step-wait-time (- step-wait-time delta-time))
                    current-state)
            ))
    ))

;; Función que se llama cada cuadro para generar un nuevo estado.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
(define (on-frame-secuential state frames delta)
    (cond
        (program-done
            current-state)
        ((null? current-state)
            (update_cube state cube-size delta))
        (else 
            (check-state delta)) 
    ))

;; Función para redibujar lo que muestra la interfaz.
;; @param state: Estado actual del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el último redibujado.
(define (on-draw-secuential state frames delta)
    (combine
        (cond 
            (program-done
                (set! delta-time (get-delta prev-time delta))
                (set! prev-time delta)
                (set! camera-rotation (+ camera-rotation (/ delta-time 20)))
                (rotate-z (get_lights+camera) camera-rotation))
            (else 
                (rotate-z (get_lights+camera) camera-rotation))
        )
        current-state
    ))
