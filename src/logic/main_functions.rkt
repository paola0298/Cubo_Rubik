#lang racket
(require racket/include)
(include "cube_state.rkt")
(include "basic_functions.rkt")
(include "rotation_functions.rkt")

;; ######## Funciones principales ########

;; Funcion para obtener en una lista la secuencia de un movimiento
;; @param step: string con el paso a realizar
(define (get_step_list step)
    (get_step_list_aux step 0 1))

(define (get_step_list_aux step start end)
    (cond
        ((equal? start 3)
            '())
        (else 
            (cons 
                (substring step start end)
                (get_step_list_aux step (+ start 1) (+ end 1))))
    ))

(define (get_dir dir)
    (cond 
        ((or (equal? dir "I") (equal? dir "A"))
            #t)
        (else #f)
    ))

;; Filas: I - D -> #t - #f
;; Columnas: A - B -> #f - #t


;; Funcion para realizar un movimiento segun lo indicado
;; @param mov: Indica si se rota fila o columna.
;; @param index: Indica el indice de la fila o columna a rotar
;; @param dir: Direccion del movimiento
;; @param n: Tamaño del cubo
;; @param cube: Estado del cubo
(define (make_movement mov index dir n cube)
    (cond 
        ((equal? mov "F") ;Hacer movimiento de filas
            ;(write dir)
            (rotate_row n index dir cube))
        (else
            ;(write dir)
            (rotate_col index dir n cube)) ; Hacer movimiento de columnas
    )) 

;; Función que se utiliza para solo procesar los pasos una vez.
;; @param steps: Lista de pasos calculada.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
(define (prepare_movement steps n cube)
    (make_movement
        (car steps)
        (string->number (cadr steps))
        (get_dir (caddr steps))
        n
        cube
    ))

;; Función principal del simulador de cubo Rubik.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
;; @param steps: Movimientos a aplicar al cubo.
(define (RS n cube steps)
    (cond 
        ((null? steps)
            cube)
        (else
            (RS
                n
                (prepare_movement
                    (get_step_list (car steps))
                    n
                    cube)
                (cdr steps)))
    ))

(RS 3 cube3x3alt '("C0A" "C1A"))