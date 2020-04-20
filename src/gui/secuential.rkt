#lang racket
(require
    racket/include
    pict3d
    pict3d/universe)

(include "../logic/cube_state.rkt")
(include "scene_constants.rkt")
(include "scene_config.rkt")

;; Función que devuelve la coordenada inicial para generar el cubo base.
;; @param n: Tamaño del cubo a generar. 
(define (get_start_coord n)
    (list (- 1 n) (- 1 n) (- 1 n)))

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
                (cube (list_to_coords position) base-cube-size) 
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

;; Función que actualiza el estado del cubo dependiendo del estado anterior.
;; @param state: Estado anterior del cubo.
;; @param cube_size: Tamaño del cubo.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
(define (update_cube state cube_size delta)
    (cond 
        (update_state
            (print "___UPDATING___")
            (set! update_state #F)
            (set! current-state 
                (gen_base_cube (get_start_coord cube_size) cube_size 0))
            current-state)
        (else 
            (print "___NOT_UPDATING___")
            (set! current-state state)
            current-state)
    ))

;; Función que se llama cada cuadro para generar un nuevo estado.
;; @param state: Estado anterior del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el cuadro anterior en milisegundos.
(define (on-frame state frames delta)
    (update_cube state 7 delta))

;; Función para redibujar lo que muestra la interfaz.
;; @param state: Estado actual del cubo.
;; @param frames: Cuadros transcurridos desde el inicio del programa.
;; @param delta: Tiempo transcurrido desde el último redibujado.
(define (on-draw state frames delta)
    (combine
        (rotate-z lights-camera (/ delta 70))
        coords
        current-state
    ))

;; Iniciación del programa
(big-bang3d 0 
    #:name "Rubik's simulator" 
    #:display-mode 'fullscreen
    #:frame-delay (/ 1000 60)
    #:on-frame on-frame
    #:on-draw on-draw)