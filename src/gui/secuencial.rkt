#lang racket
(require 
    racket/include
    pict3d
    pict3d/universe)

(include "../logic/cube_state.rkt")

(current-material 
    (material 
        #:ambient 0.5
        #:diffuse 0.4
        #:specular 0.1
        #:roughness 0.5))

;; Cámara de la escena
(define lights-camera
    (combine 
        (light (pos 5 0 0) (emitted "white" 25))
        (light (pos 0 5 0) (emitted "white" 25))
        (light (pos 0 0 5) (emitted "white" 25))
        (basis 'camera (point-at (pos 5 5 4) origin))))

;; Ejes de coordenadas para referencia
(define coords
    (combine
        (with-color (rgba "red") (arrow origin (pos 0 0 4)))
        (with-color (rgba "green") (arrow origin (pos 0 4 0)))
        (with-color (rgba "blue") (arrow origin (pos 4 0 0)))
    ))

;; Variable que define el incremento de coordenadas entre cubo y cubo
(define coord-step 2);

;; Variable para almacenar los cubos base del cubo
(define base-cube '())


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


;; Funciones para generar la matriz de coordenadas de cada cara
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

;; Función para multiplicar las coordenadas dadas por un número dado
(define (multiply_coords coords multiplier)
    (list
        (* (car coords) multiplier)
        (* (cadr coords) multiplier)
        (* (caddr coords) multiplier) 
    ))

;; Función para calcular las coordenadas de las caras del cubo
(define (calculate_faces_coords)
    (#t))

;; Función que se llama cada cuadro para actualizar los gráficos
(define (on-draw s n t)
    (combine
        (rotate-z lights-camera (/ t 90))
        
        (with-color (rgba "red") (cube (pos 1 1 1) 1))
        (with-color (rgba "green") (cube (pos -1 -1 1) 1))
        (with-color (rgba "blue") (cube (pos 1 -1 1) 1))
        (with-color (rgba "white") (cube (pos -1 1 1) 1))

        (with-color (rgba "white") (cube (pos 1 1 3) 1))
        (with-color (rgba "blue") (cube (pos -1 -1 3) 1))
        (with-color (rgba "green") (cube (pos 1 -1 3) 1))
        (with-color (rgba "red") (cube (pos -1 1 3) 1))

        (with-color (rgba "red") (arrow origin (pos 0 0 4)))
        (with-color (rgba "green") (arrow origin (pos 0 4 0)))
        (with-color (rgba "blue") (arrow origin (pos 4 0 0)))
    ))

;; Llamada a la función principal
(big-bang3d 0 #:on-draw on-draw)