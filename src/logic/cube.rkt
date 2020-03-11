#lang racket

;; Cube face
;; (selected (row1 row2 ... rowN))
;; (#t (("B" "B" "B") ("B" "B" "B") ("B" "B" "B")))
;; 
;; Cube state
;; (face1 face2 ... face6)
;;
;; 0 - 3: sides of cube
;; 4 - 5: top & bottom

(define cube
    '(
        (#F (("B1" "B1" "B1")
             ("B2" "B2" "B2")
             ("B3" "B3" "B3")))
        (#T (("W1" "W1" "W1")
             ("W2" "W2" "W2")
             ("W3" "W3" "W3")))
        (#F (("G1" "G1" "G1")
             ("G2" "G2" "G2")
             ("G3" "G3" "G3")))
        (#F (("Y1" "Y1" "Y1")
             ("Y2" "Y2" "Y2")
             ("Y3" "Y3" "Y3")))
        (#F (("P1" "P1" "P1")
             ("P2" "P2" "P2")
             ("P3" "P3" "P3")))
        (#F (("O1" "O1" "O1")
             ("O2" "O2" "O2")
             ("O3" "O3" "O3")))
    ))

;; Función principal.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
;; @param steps: Movimientos a aplicar al cubo.
(define (RS n cube steps) 
    (cond 
        ((zero? n) #F)
        (else #T)
        ))

;; Función para obtener la cara seleccionada.
;; @param cube: Estado del cubo. 
(define (actual_face? cube) 
    (cond 
        ((caar cube) (cadar cube))
        (else (actual_face? (cdr cube)))
    ))

;; Función para verificar si la cara dada está seleccionada.
;; @param face: Cara del cubo a verificar.
(define (is_face_selected? face)
    (car face))

;; Función para devolver la matriz de la cara dada.
;; @param face: Cara de donde obtener la matriz.
(define (get_face_matrix face)
    (cadr face))

;; Función para obtener una fila de una cara.
;; @param i: Posición inicial (0).
;; @param row: Índice de la fila a devolver.
;; @param face: Cara de donde obtener la fila.
(define (get_row row face)
    (cond 
        ((zero? row) (car face))
        (else (get_row (- row 1) (cdr face)))
    ))

;; Función para sobreescribir una fila.
;; @param i: Posición inicial (0).
;; @param row: Índice de la fila a sobreescribir.
;; @param new_row: Fila a escribir.
;; @param face: Cara donde sobreescribir la fila.
(define (set_row row new_row face)
    (cond 
        ((null? face) '())
        ((zero? row) (cons new_row (set_row (- row 1) new_row (cdr face))))
        (else (cons (car face) (set_row (- row 1) new_row (cdr face))))
    ))

;; Función para rotar una fila del cubo.
;; @param row: Índice de la fila a rotar.
;; @param cw: Boleano que indica si la rotación es en el sentido de las agujas del reloj o al revés.
;; @param cube: Estado del cubo.
(define (rotate_row row cw cube)
    (cond 
        (cw ;; Rotar hacia el sentido de las agujas del reloj.
            (cond 
                ()
            )
            (apply_list row (first_to_last (get_all_rows row cube)) cube)
        )
        (else 
            (apply_list row (last_to_first (get_all_rows row cube)) cube)
        )
    ))

;; Función para obtener la fila seleccionada de todas las caras laterales.
;; @param row: Índice de la fila a obtener.
;; @param cube: Estado de cubo.
(define (get_all_rows row cube)
    (get_all_rows_aux 4 row cube))
    
(define (get_all_rows_aux i row cube)
    (cond 
        ((zero? i) 
            '())
        (else 
            (cons (get_row row (get_face_matrix (car cube)))
                  (get_all_rows_aux (- i 1) row (cdr cube))))
    ))

;; Función para poner el primer elemento de último.
;; @param rows: Lista de filas.
(define (first_to_last rows)
    (append (cdr rows) 
            (list (car rows))))

;; Función para poner el último elemento de primero.
;; @param rows: Lista de filas.
(define (last_to_first rows)
    (reverse (first_to_last (reverse rows))))

(define (reverse l)
    (cond
        ((null? l) '()) 
        ((append (reverse (cdr l)) (list (car l))))
    ))

;; Función para aplicar la lista de filas al cubo.
;; @param row: Índice de la fila.
;; @param rows: Lista de filas.
;; @param cube: Estado del cubo.
(define (apply_list row rows cube)
    (cond 
        ((null? rows) 
            cube)
        (else 
            (cons 
                (append 
                    (list (is_face_selected? (car cube))) 
                    (list (set_row row (car rows) (get_face_matrix (car cube)))))
                (apply_list row (cdr rows) (cdr cube))))
    ))

;; (rotate_row 1 #f cube)

(define (rotate_matrix_cw matrix)
    ())


(define (col_to_row matrix)
    (cond 
        ((null? matrix) 
            '())
        (else 
            (cons (caar matrix) (col_to_row (cdr matrix))))
    ))

(define (delete_col matrix)
    (cond 
        ((null? matrix) '())
        (else 
            (cons (cdar matrix) (delete_col (cdr matrix))))
    ))

