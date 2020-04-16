#lang racket
(require 
    racket/include
    pict3d
    pict3d/universe)

(include "../logic/cube_state.rkt")
(include "scene_constants.rkt")
(include "scene_config.rkt")

;; Funciones para modificar las coordenadas dadas en la dirección especificada
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

;; Función para multiplicar las coordenadas dadas por un número dado
(define (multiply_coords coords multiplier)
    (list
        (* (car coords) multiplier)
        (* (cadr coords) multiplier)
        (* (caddr coords) multiplier) 
    ))

;; Función para calcular las coordenadas de las caras del cubo (BASE)
(define (calculate_cube_coords)
    (#t))

;; Funciones para generar la matriz de coordenadas de cada cara (BASE)
(define (gen_top_coords)
    (#f))

(define (gen_bottom_coords)
    (#f))

(define (gen_left_coords)
    (#f))

(define (gen_right_coords)
    (#f))

(define (gen_back_coords)
    (#f))

(define (gen_front_coords)
    (#f))

;; Función para generar los cubos de colores con una matriz de coordenadas y una matriz de colores
(define (generate_color_cubes coords_matrix face_matrix)
    (#f))

;; Función de renderizado principal
(define (render_cube cube_size)
    ())

;; Función que se llama cada cuadro para actualizar los gráficos
(define (on-draw s n t)
    (render_cube cube_size))

;; Llamada a la función principal
(big-bang3d 0 #:on-draw on-draw)