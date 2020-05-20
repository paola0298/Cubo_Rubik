;; Función que se llama cada cuadro para generar un nuevo estado.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
(define (on-frame-interactive state frames delta)
    (cond 
        (update_state
            (update_cube state cube-size delta))
        (else
            current-state)
    ))

;; Función para obtener el estado actual del cubo aplicando las rotaciones necesarias.
(define (get_cube_state)
    (rotate-z 
        (rotate-y 
            (rotate-x 
                (combine current-state) 
                cube-rotation-x) 
            cube-rotation-y) 
        cube-rotation-z))

;; Función que se llama cuando se realiza un evento con el mouse.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el inicio del programa.
;; @param x_pos: Posición en X del mouse.
;; @param x_pos: Posición en Y del mouse.
;; @param action: Acción realizada con el mouse.
(define (on-mouse-interactive state frames delta x_pos y_pos action)
    ;(printf "X: ~a Y: ~a Action: ~a\n" x_pos y_pos action)
    state)

;; Función que se llama cuando se presiona una tecla.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el inicio del programa.
;; @param key_pressed: String que indica la tecla que se presionó.
(define (on-key-interactive state frames delta key_pressed)
    ;(printf "Key pressed: ~a.\n" key_pressed)
    (check-rotation-input key_pressed)
    (check-movement-input key_pressed))

;; Función que verifica si la tecla presionada corresponde a alguna acción de movimientos.
;; @param key_pressed: String que indica la tecla que se presionó.
(define (check-movement-input key_pressed)
    (cond 
        ((equal? key_pressed "z")
            (set! movement-type-selected (not movement-type-selected)))
        ((equal? key_pressed "x")
            (set! movement-direction (not movement-direction)))
        ((string-numeric? key_pressed)
            (let* ([temp-index (string->number key_pressed)])
                (cond 
                    ((> temp-index (- cube-size 1))
                        (set! movement-index (- cube-size 1)))
                    (else
                        (set! movement-index (string->number key_pressed)))
                )))
        ((equal? key_pressed "c")
            (set! cube-internal-state 
                (make_interactive_movement 
                    movement-type-selected 
                    movement-direction 
                    movement-index 
                    cube-size 
                    cube-internal-state))
            (set! update_state #T))
    )
    (show_program_info))

;; Función que muestra la información del estado actual del programa en la consola.
(define (show_program_info)
    (let* ([type (cond 
                    (movement-direction "Left/Up")
                    (else "Right/Down"))])
        (cond 
            (movement-type-selected
                (printf "Mode: Columns | Direction: ~a | Index: ~a\n" type movement-index))
            (else
                (printf "Mode: Rows | Direction: ~a | Index: ~a\n" type movement-index))
        )
    ))

;; Función que verifica si la tecla presionada corresponde a una acción de rotación.
;; @param key_pressed: String que indica la tecla que se presionó.
(define (check-rotation-input key_pressed)
    (cond 
        ((equal? key_pressed "w")
            (set! cube-rotation-x (+ cube-rotation-x 2.5)))
        ((equal? key_pressed "s")
            (set! cube-rotation-x (- cube-rotation-x 2.5)))
        ((equal? key_pressed "a")
            (set! cube-rotation-z (- cube-rotation-z 2.5)))
        ((equal? key_pressed "d")
            (set! cube-rotation-z (+ cube-rotation-z 2.5)))
        ((equal? key_pressed "q")
            (set! cube-rotation-y (+ cube-rotation-y 2.5)))
        ((equal? key_pressed "e")
            (set! cube-rotation-y (- cube-rotation-y 2.5)))
        ((equal? key_pressed "r")
            (set! cube-rotation-x 0.0)
            (set! cube-rotation-y 0.0)
            (set! cube-rotation-z 0.0))
    ))

;; Función que muestra la información de rotación en la consola.
(define (show_rotation_info)
    (printf "X: ~a Y: ~a Z: ~a\n" cube-rotation-x cube-rotation-y cube-rotation-z))

;; Función para redibujar lo que muestra la interfaz.
;; @param state: Estado actual del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el último redibujado.
(define (on-draw-interactive state frames delta)
    (combine
        (get_lights+camera)
        (get_cube_state)
    ))