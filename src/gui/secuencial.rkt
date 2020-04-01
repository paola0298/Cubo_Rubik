#lang racket
(require 
    pict3d
    pict3d/universe)

(define cube_state
    '(
        (#F (("B" "B" "B")
             ("B" "B" "B")
             ("B" "B" "B")))
        (#T (("W" "W" "W")
             ("W" "W" "W")
             ("W" "W" "W")))
        (#F (("G" "G" "G")
             ("G" "G" "G")
             ("G" "G" "G")))
        (#F (("Y" "Y" "Y")
             ("Y" "Y" "Y")
             ("Y" "Y" "Y")))
        (#F (("P" "P" "P")
             ("P" "P" "P")
             ("P" "P" "P")))
        (#F (("O" "O" "O")
             ("O" "O" "O")
             ("O" "O" "O")))
))


(current-material 
    (material 
        #:ambient 0.1
        #:diffuse 0.39
        #:specular 0.6
        #:roughness 0.2))

(define lights-camera
    (combine 
        (light (pos 0 1 2) (emitted "white" 7))
        (light (pos 0 -1 -2) (emitted "white" 7))
        (basis 'camera (point-at (pos 7 7 6) origin))))

(define (create-corner x y z)
    (#F))

(define (create-edge x y z)
    (#F))

(define (create-center x y z)
    (#F))

(define (create-row n)
    (#F))

(define (calc_start_pos n)
    (list (- 1 n) (- 1 n) (- 1 n)))

(define (combine_list l)
    (cond 
        ((null? (cdr l)) 
            (car l))
        (else 
            (combine (car l) (combine_list (cdr l))))
    ))

(define (start_render n cube movements)
    (combine_list (start_render_aux (- 1 n) (- 1 n) (- 1 n) n cube movements)))

(define (start_render_aux x y z n cube movements)
    (cond 
        ((zero? z) ; Fila 0
            ())
        ((equal? (- n 1) z) ; Fila (n - 1) 
            ())
        (else   ; Resto de filas
            ())
    ))

(define (on-draw s n t)
    (combine 
        (rotate-z lights-camera (/ t 100))

        (start_render 3 cube_state '())
    ))

(big-bang3d 0 #:on-draw on-draw)