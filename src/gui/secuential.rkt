;#lang racket
;(require
;    racket/include
;    pict3d
;    pict3d/universe)

;(include "../logic/cube_state.rkt")
;(include "scene_constants.rkt")
;(include "scene_config.rkt")

;; Función para crear las luces y la cámara de la escena actual.
(define (get_lights+camera)
    (combine
        (light (pos (+ cube-size 8) 0 0) (emitted "white" 30))
        (light (pos 0 (+ cube-size 8) 0) (emitted "white" 30))
        (light (pos 0 0 (+ cube-size 8)) (emitted "white" 30))
        (basis 'camera (point-at (pos (+ cube-size 5) (+ cube-size 5) (+ cube-size 2)) origin))
    ))

;; Función que devuelve la coordenada inicial para generar el cubo base.
;; @param n: Tamaño del cubo a generar. 
(define (get_start_coord n)
    (list (- 1 n) (- 1 n) (- 1 n)))

;; Función que devuelve la coordenada opuesta a la inicial para generar el cubo.
;; @param n: Tamaño del cubo.
(define (get_opp_coord n)
    (list (- n 1) (- n 1) (- n 1)))

;; Función que convierte una lista con coordenadas a un objeto de posición.
;; @param l: Lista con coordenadas. 
(define (list_to_coords l)
    (pos (car l) (cadr l) (caddr l)))

;; Funciones para modificar las coordenadas dadas en la dirección especificada.
;; @param coords: Lista de coordenadas a modificar.
(define (coord_up coords)
    (list (car coords) (cadr coords) (+ (caddr coords) coord-step)))
(define (coord_down coords)
    (list (car coords) (cadr coords) (- (caddr coords) coord-step)))
(define (coord_left coords)
    (list (+ (car coords) coord-step) (cadr coords) (caddr coords)))
(define (coord_right coords)
    (list (- (car coords) coord-step) (cadr coords) (caddr coords)))
(define (coord_back coords)
    (list (car coords) (- (cadr coords) coord-step) (caddr coords)))
(define (coord_front coords)
    (list (car coords) (+ (cadr coords) coord-step) (caddr coords)))

(define (coord_up_color coords)
    (list (car coords) (cadr coords) (+ (caddr coords) coord-step-color)))
(define (coord_down_color coords)
    (list (car coords) (cadr coords) (- (caddr coords) coord-step-color)))
(define (coord_left_color coords)
    (list (+ (car coords) coord-step-color) (cadr coords) (caddr coords)))
(define (coord_right_color coords)
    (list (- (car coords) coord-step-color) (cadr coords) (caddr coords)))
(define (coord_back_color coords)
    (list (car coords) (- (cadr coords) coord-step-color) (caddr coords)))
(define (coord_front_color coords)
    (list (car coords) (+ (cadr coords) coord-step-color) (caddr coords)))



;; Funcion para obtener la letra del color del cubo
;; @param word: letra del color
(define (get_initial_color word)
    (substring word 0 1))

;; Función que convierte el color del estado del cubo en un color
;; @param color: string del cubo en la lista
(define (word_to_color color)
    (cond 
        ((equal? color "W") "white")
        ((equal? color "B") "blue")
        ((equal? color "Y") "yellow")
        ((equal? color "G") "green")
        ((equal? color "P") "pink")
        ((equal? color "O") "orange")
        ((equal? color "R") "red")
    ))

;; Función para generar una fila de cubos base.
;; @param position: posición inicial de donde generar la fila.
;; @param n: Tamaño de cubo.
;; @param i: Contador.
(define (gen_base_row position n i)
    (cond 
        ((equal? i n)
            '())
        (else 
            (cons 
                (with-color (rgba "black") (cube (list_to_coords position) base-cube-size)) 
                (gen_base_row (coord_front position) n (+ i 1))))
    ))

;; Función para generar una columna de cubos base.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
(define (gen_base_col position n i)
    (cond 
        ((equal? i n)
            '())
        (else 
            (cons 
                (gen_base_row position n 0)
                (gen_base_col (coord_up position) n (+ i 1))))
    ))

;; Función para generar una fila con los colores de la cara 0.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param row: Lista con los colores de la fila.
(define (gen_color_row_0 position n i row)
    (cond
        ((equal? i n)
            '())
        (else
            (cons
                (with-color (rgba (word_to_color (get_initial_color (car row)))) (cube (list_to_coords position) color-cube-size))
                (gen_color_row_0 (coord_front position) n (+ i 1) (cdr row)))))) 

;; Función para generar una fila con los colores de la cara 1, 4 y 5.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param row: Lista con los colores de la fila.
(define (gen_color_row_1_4_5 position n i row)
    (cond
        ((equal? i n)
            '())
        (else
            (cons
                (with-color (rgba (word_to_color (get_initial_color (car row)))) (cube (list_to_coords position) color-cube-size))
                (gen_color_row_1_4_5 (coord_right position) n (+ i 1) (cdr row))))))

;; Función para generar una fila con los colores de la cara 2.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param row: Lista con los colores de la fila.
(define (gen_color_row_2 position n i row)
    (cond
        ((equal? i n)
            '())
        (else
            (cons   
                (with-color (rgba (word_to_color (get_initial_color (car row)))) (cube (list_to_coords position) color-cube-size))
                (gen_color_row_2 (coord_back position) n (+ i 1) (cdr row))))))

;; Función para generar una fila con los colores de la cara 3.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param row: Lista con los colores de la fila.
(define (gen_color_row_3 position n i row)
    (cond
        ((equal? i n)
            '())
        (else
            (cons
                (with-color (rgba (word_to_color (get_initial_color (car row)))) (cube (list_to_coords position) color-cube-size))
                (gen_color_row_3 (coord_left position) n (+ i 1) (cdr row))))))

;; Función para generar una columna con los colores de la cara 0.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_0 position n i face)
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_0 position n 0 (car face))
                (gen_color_col_0 (coord_down position) n (+ i 1) (cdr face))))))

;; Función para generar una columna con los colores de la cara 1.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_1 position n i face) 
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_1_4_5 position n 0 (car face))
                (gen_color_col_1 (coord_down position) n (+ i 1) (cdr face))))))

;; Función para generar una columna con los colores de la cara 2.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_2 position n i face) 
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_2 position n 0 (car face))
                (gen_color_col_2 (coord_down position) n (+ i 1) (cdr face))))))

;; Función para generar una columna con los colores de la cara 3.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_3 position n i face) 
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_3 position n 0 (car face))
                (gen_color_col_3 (coord_down position) n (+ i 1) (cdr face))))))

;; Función para generar una columna con los colores de la cara 4.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_4 position n i face) 
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_1_4_5 position n 0 (car face))
                (gen_color_col_4 (coord_front position) n (+ i 1) (cdr face))))))

;; Función para generar una columna con los colores de la cara 5.
;; @param position: Posición inicial de donde generar la columna.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param face: Matriz con las filas de la cara. 
(define (gen_color_col_5 position n i face) 
    (cond 
        ((equal? i n) 
            '())
        (else 
            (cons 
                (gen_color_row_1_4_5 position n 0 (car face))
                (gen_color_col_5 (coord_back position) n (+ i 1) (cdr face))))))

;; Función para generar los cubos base.
;; @param position: Posición inicial de donde generar los cubos.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
(define (gen_base_cube position n i)
    (cond 
        ((equal? i n) '())
        (else 
            (cons 
                (gen_base_col position n 0)
                (gen_base_cube (coord_left position) n (+ i 1))))
    ))

;; Función para generar los cubos de colores.
;; @param position: Posición inicial de donde generar los cubos.
;; @param n: Tamaño del cubo.
;; @param i: Contador.
;; @param cube: Estado interno del cubo.

(define (gen_color_cube i n cube)
    (cond 
        ((equal? i 6) '())
        ((equal? i 0)
            (cons
                (gen_color_col_0 (coord_left_color(coord_back (coord_back (get_opp_coord n)))) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
        ((equal? i 1)
            (cons
                (gen_color_col_1 (coord_front_color (get_opp_coord n)) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
        ((equal? i 2)
            (cons 
                (gen_color_col_2 (coord_right_color (coord_right (coord_right (get_opp_coord n)))) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
        ((equal? i 3)
            (cons 
                (gen_color_col_3 (coord_back_color (coord_up (coord_up (get_start_coord n)))) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
        ((equal? i 4)
            (cons
                (gen_color_col_4 (coord_up_color (coord_back (coord_back (get_opp_coord n)))) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
        ((equal? i 5)
            (cons 
                (gen_color_col_5 (coord_down_color (coord_down (coord_down (get_opp_coord n)))) n 0 (cadar cube))
                (gen_color_cube (+ i 1) n (cdr cube))))
            
    ))

(define (gen_color_cube_2 i n cube)
    (append
        (gen_color_col_0 (coord_left_color(coord_back (coord_back (get_opp_coord n)))) n 0 (cadar cube)) ;funciona
        (gen_color_col_1 (coord_front_color (get_opp_coord n)) n 0 (cadadr cube))
        (gen_color_col_2 (coord_right_color (coord_right (coord_right (get_opp_coord n)))) n 0 (cadar(cddr cube)))
        (gen_color_col_3 (coord_back_color (coord_up (coord_up (get_start_coord n)))) n 0 (cadar(cdddr cube)))
        (gen_color_col_4 (coord_up_color (coord_back (coord_back (get_opp_coord n)))) n 0 (cadar (cddddr cube)))
        (gen_color_col_5 (coord_down_color (coord_down (coord_down (get_opp_coord n)))) n 0 (cadar (cdr (cddddr cube3x3))))
    ))
;; Función que actualiza el estado del cubo dependiendo del estado anterior.
;; @param state: Estado anterior del cubo.
;; @param cube_size: Tamaño del cubo.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
;; cube-internal-state, variable que almacena el estado del cubo
(define (update_cube state cube_size delta)
    (cond 
        (update_state
            ;(print "___UPDATING___")
            (set! update_state #F)
            (set! current-state 
                (combine
                    (gen_base_cube (get_start_coord cube_size) cube_size 0)
                    (gen_color_cube_2 0 cube-size cube_prueba) 
                    ;(gen_color_col_1 (coord_front_color(get_opp_coord cube-size)) cube-size 0 '(("W1" "W1" "W1")("W2" "W2" "W2")("W3" "W3" "W3")))
                ))
            current-state)
        (else 
            ;(print "___NOT_UPDATING___")
            (set! current-state state)
            current-state)
    ))

;; Función que se llama cada cuadro para generar un nuevo estado.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
(define (on-frame state frames delta)
    (update_cube state cube-size delta))

;; Función para redibujar lo que muestra la interfaz.
;; @param state: Estado actual del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el último redibujado.
(define (on-draw state frames delta)
    (combine
        ;(rotate-z lights-camera (/ delta 70))
        (rotate-z (get_lights+camera) (/ delta 70))
        coords
        current-state
    ))

;; Iniciación del programa

;(big-bang3d 0 
;    #:name "Rubik's simulator" 
;    #:display-mode 'fullscreen
;    #:frame-delay (/ 1000 60)
;    #:on-frame on-frame
;    #:on-draw on-draw)