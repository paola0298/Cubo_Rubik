;(include "basic_functions.rkt")
;(include "rotation_functions.rkt")

;; ######## Funciones principales ########

;; Funcion para convertir un paso a una lista para procesarse
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

;; Función para obtener la dirección de rotación.
;; @param dir: Dirección sin procesar.
(define (get_dir dir)
    (cond 
        ((or (equal? dir "I") (equal? dir "A"))
            #t)
        (else #f)
    ))

;; Funcion para aplicar un movimiento dado en el cubo
;; @param mov: Indica si se rota fila o columna.
;; @param index: Indica el indice de la fila o columna a rotar
;; @param dir: Direccion del movimiento
;; @param n: Tamaño del cubo
;; @param cube: Estado del cubo
(define (make_movement mov index dir n cube)
    (cond 
        ((equal? mov "F")
            (rotate_row n index dir cube))
        (else
            (rotate_col index dir n cube))
    )) 

;; Función que se utiliza para solo procesar los pasos una vez.
;; @param steps: Lista de pasos calculada.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
(define (prepare_movement step n cube)
    (make_movement
        (car step)
        (string->number (cadr step))
        (get_dir (caddr step))
        n
        cube
    ))

;; Función que procesa el paso a realizar y lo aplica en el cubo
;; @param raw-step: String del paso a realizar.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo. 
(define (do-step raw-step n cube)
    (prepare_movement
        (get_step_list raw-step)
        n
        cube
    ))

;; Función principal que aplica una lista de rotaciones al cubo rubik.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
;; @param steps: Lista de movimientos a aplicar al cubo.
(define (RS n cube steps)
    (cond 
        ((null? steps)
            cube)
        (else
            (RS 
                n 
                (do-step (car steps) n cube) 
                (cdr steps)))
    ))
