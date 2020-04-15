;#lang racket
;(require racket/include)
;(include "cube_state.rkt")
;(include "basic_functions.rkt")

;; ######## Funciones para rotar filas ########

;; Función para obtener una fila de una cara.
;; @param i: Posición inicial (0).
;; @param row: Índice de la fila a devolver.
;; @param face: Cara de donde obtener la fila.
(define (get_row row matrix)
    (cond 
        ((zero? row) 
            (car matrix))
        (else 
            (get_row (- row 1) (cdr matrix)))
    ))

;; Función para sobreescribir una fila.
;; @param i: Posición inicial (0).
;; @param row: Índice de la fila a sobreescribir.
;; @param new_row: Fila a escribir.
;; @param face: Cara donde sobreescribir la fila.
(define (set_row row new_row matrix)
    (cond 
        ((null? matrix) 
            '())
        ((zero? row) 
            (cons 
                new_row 
                (set_row (- row 1) new_row (cdr matrix))))
        (else 
            (cons 
                (car matrix) 
                (set_row (- row 1) new_row (cdr matrix))))
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

;; Función para acomodar la lista de filas según la dirección de la rotación.
;; @param left: Boleano que indica si la rotación es hacia la izquierda o derecha.
;; @param rows: Lista de filas a ordenar. 
(define (sort_lists left rows)
    (cond 
        (left 
            (first_to_last rows))
        (else 
            (last_to_first rows))
    ))

;; Función para poner el primer elemento de una lista de último.
;; @param rows: Lista de filas.
(define (first_to_last rows)
    (append (cdr rows) 
            (list (car rows))))

;; Función para poner el último elemento de una lista de primero.
;; @param rows: Lista de filas.
(define (last_to_first rows)
    (reverse_list (first_to_last (reverse_list rows))))

;; Función para darle vuelta a una lista.
(define (reverse_list l)
    (cond
        ((null? l) '()) 
        ((append (reverse_list (cdr l)) (list (car l))))
    ))

;; Función para aplicar la lista de filas al cubo.
;; @param row: Índice de la fila.
;; @param rows: Lista de filas.
;; @param cube: Estado del cubo.
(define (apply_rows_list row rows cube)
    (cond 
        ((null? rows) 
            cube)
        (else 
            (cons 
                (append 
                    (list (is_face_selected? (car cube))) 
                    (list (set_row row (car rows) (get_face_matrix (car cube)))))
                (apply_rows_list row (cdr rows) (cdr cube))))
    ))

;; Función para obtener la primer columna de una matriz.
;; @param matrix: Matriz de entrada.
(define (col_to_row matrix)
    (cond 
        ((null? matrix) 
            '())
        (else 
            (cons 
                (caar matrix) 
                (col_to_row (cdr matrix))))
    ))

;; Función que elimina la primer columna de una matriz.
;; @param matrix: Matriz de entrada.
(define (delete_col matrix)
    (cond 
        ((null? matrix) 
            '())
        ((null? (cdar matrix)) 
            (delete_col (cdr matrix)))
        (else 
            (cons (cdar matrix) (delete_col (cdr matrix))))
    ))

;; Función para rotar una matriz hacia la izquierda o derecha.
;; @param left: Boleano que indica hacia que lado rotar.
;; @param matrix: Matriz a rotar.
(define (rotate_matrix left matrix)
    (cond 
        ((null? matrix) 
            '())
        (left 
            (append 
                (list (reverse_list (col_to_row matrix)))
                (rotate_matrix left (delete_col matrix))))
        (else 
            (append 
                (rotate_matrix left (delete_col matrix))
                (list (col_to_row matrix))))
    ))

;; Función para rotar una cara hacia la izquierda o derecha.
;; @param left: Boleano que indica hacia que lado rotar.
;; @param index: Índice de la cara a rotar.
;; @param cube: Estado del cubo.
(define (rotate_face left index cube)
    (cond 
        ((null? cube) 
            '())
        ((zero? index) 
            (cons 
                (append 
                    (list (is_face_selected? (car cube)))
                    (list (rotate_matrix left (get_face_matrix (car cube)))))
                (rotate_face left (- index 1) (cdr cube))))
        (else 
            (cons (car cube) (rotate_face left (- index 1) (cdr cube))))
    ))


;; Función para rotar una fila del cubo.
;; @param n: Tamaño del cubo
;; @param row: Índice de la fila a rotar.
;; @param left: Boleano que indica si la rotación es hacia la izquierda o derecha.
;; @param cube: Estado del cubo.
(define (rotate_row n row left cube)
    (cond 
        ((zero? row) 
            (rotate_face left 4 (apply_rows_list row (sort_lists left (get_all_rows row cube)) cube)))
        ((equal? (- n 1) row) 
            (rotate_face (not left) 5 (apply_rows_list row (sort_lists left (get_all_rows row cube)) cube)))
        (else 
            (apply_rows_list row (sort_lists left (get_all_rows row cube)) cube))
    ))

;; ######## Funciones para rotar columnas ########

;; Funcion para obtener una columna en cierto indice dado
;; @param i: indice de la columna a obtener
;; @param matrix: matriz/cara de donde obtener la columna
(define (get_column i matrix)
    (cond 
        ((null? matrix)
            '())
        (else 
            (cons
                (get_element_at 0 i (car matrix))
                (get_column i (cdr matrix))))
    ))
(define (get_element_at i index l)
    (cond 
        ((null? l) 
            #f)
        ((equal? i index)
            (car l))
        (else
            (get_element_at (+ i 1) index (cdr l)))
    ))


;; Funcion para obtener todas las columnas de la posición dada.
;; @param index: indice de la columna a obtener
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo
(define (get_all_cols index n cube)
    (get_all_cols_aux 4 index n cube))

(define (get_all_cols_aux i index n cube)
    (cond 
        ((zero? i) 
            '())
        ((equal? i 4) ;FRONT
            (cons 
                (get_column index 
                    (get_face_matrix (actual_face cube)))
                (get_all_cols_aux (- i 1) index n cube)))
        ((equal? i 3) ;TOP
            (cons 
                (get_column index 
                    (get_face_matrix (get_top cube)))
                (get_all_cols_aux (- i 1) index n cube)))
        ((equal? i 2) ;BACK
            (cons 
                (get_column (- (- n 1) index) 
                    (get_face_matrix (get_back_of (actual_face cube) cube)))
                (get_all_cols_aux (- i 1) index n cube)))
        ((equal? i 1) ;BOTTOM
            (cons 
                (get_column index 
                    (get_face_matrix (get_bottom cube)))
                (get_all_cols_aux (- i 1) index n cube)))
    ))

;; Funcion para reemplazar una columna
;; @param col: indice de la columa
;; @param new_col: la columna a escribir
;; @param matrix: cara donde se escribira la nueva columna
(define (set_column index col matrix)
    (cond 
        ((null? matrix) 
            '())
        (else 
            (cons 
                (replace_element_at 0 index (car col) (car matrix))
                (set_column index (cdr col) (cdr matrix))))
    ))
(define (replace_element_at i index item l)
    (cond 
        ((null? l) 
            '())
        ((equal? i index) 
            (cons 
                item
                (replace_element_at (+ i 1) index item (cdr l))))
        (else 
            (cons 
                (car l)
                (replace_element_at (+ i 1) index item (cdr l))))
    ))

;; Funcion para aplicar la lista de columnas al cubo
;; @param col: indice de la columna
;; @param columns: lista de columnas
;; @param cube: estado del cubo
;; @param n: tamaño del cubo
(define (apply_col_list col columns cube n)
    (apply_col_list_aux 5 col columns cube n))

(define (apply_col_list_aux i col columns cube n)
    (cond 
        ((zero? i) ;bottom
            (cons
                (append 
                    (list (is_face_selected? (car cube)))
                    (list (set_column col (cadr columns) (get_face_matrix (car cube)))))
                '()))
        ((equal? i 1) ;top
            (cons 
                (append
                    (list (is_face_selected? (car cube)))
                    (list (set_column col (car columns) (get_face_matrix (car cube)))))
                (apply_col_list_aux (- i 1) col (cdr columns) (cdr cube) n)))
        ((equal? i 2) ;back_of
            (cons 
                (append
                    (list (is_face_selected? (car cube)))
                    (list (set_column (- (- n 1) col) (cadr columns) (get_face_matrix (car cube)))))
                (apply_col_list_aux (- i 1) col columns (cdr cube) n)))
        ((equal? i 4) ;actual face
            (cons 
                (append
                    (list (is_face_selected? (car cube)))
                    (list (set_column col (car columns) (get_face_matrix (car cube)))))
                (apply_col_list_aux (- i 1) col (cdr columns) (cdr cube) n)))
        (else 
            (cons 
                (car cube)
                (apply_col_list_aux (- i 1) col columns (cdr cube) n)))
    ))


;; Funcion para cambiar el orden de los elementos de la lista de columnas
;; segun hacia donde es el movimiento
;; @param cols: lista de columnas
;; @param up: determina si el movimiento es hacia arriba o abajo, #t arriba #f abajo 
(define (prepare_cols cols up)
    (cond 
        (up
            (reverse_cols 0 (last_to_first cols) 2 3))
        (else
            (reverse_cols 0 (first_to_last cols) 1 2))
    ))

;; Función para invertir 2 sublistas dadas de una lista
;; @param i: Índice inicial.
;; @param cols: Lista de columnas.
;; @param c1: Sublista 1 a invertir.
;; @param c2: Sublista 2 a invertir.
(define (reverse_cols i cols c1 c2)
    (cond 
        ((null? cols) 
            '())
        ((or (equal? i c1) (equal? i c2))
            (cons 
                (reverse_list (car cols))
                (reverse_cols (+ i 1) (cdr cols) c1 c2)))
        (else
            (cons
                (car cols)
                (reverse_cols (+ i 1) (cdr cols) c1 c2)))
    ))

;; Funcion para rotar una columna del cubo
;; @param index: índice de la columna a rotar
;; @param up: Boleano que indica si la rotación es hacía arriba o abajo. #t es arriba, #f es abajo
;; @param n: tamaño del cubo
;; @param cube: Estado del cubo
(define (rotate_col index up n cube)
    (cond 
        ((zero? index)
            (rotate_face (not up) 0 
                (apply_col_list index (prepare_cols (get_all_cols index n cube) up) cube n)
            ))
        ((equal? index (- n 1))
            (rotate_face up 2
                (apply_col_list index (prepare_cols (get_all_cols index n cube) up) cube n)
            ))
        (else
            (apply_col_list index (prepare_cols (get_all_cols index n cube) up) cube n))
    ))
