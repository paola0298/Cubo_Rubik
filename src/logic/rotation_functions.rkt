#lang racket
(require racket/include)

(include "cube_state.rkt")
(include "basic_functions.rkt")
(include "test_var.rkt")


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
    (reverse (first_to_last (reverse rows))))

;; Función para darle vuelta a una lista.
(define (reverse l)
    (cond
        ((null? l) '()) 
        ((append (reverse (cdr l)) (list (car l))))
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
                (list (reverse (col_to_row matrix)))
                (rotate_matrix left (delete_col matrix))))
        (else 
            (append 
                (rotate_matrix left (delete_col matrix))
                (list (col_to_row matrix))))
    ))

;; Función para rotar una cara hacia la izquierda o derecha.
;; @param left: Boleano que indica hacia que lado rotar
;; @param face: Cara del cubo a rotar.
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

;; Función para imprimir solo las matrices de un cubo.
;; @param cube: Cubo a imprimir.
(define (pc cube)
    (cond 
        ((null? cube) 
            '())
        (else 
            (cons (get_face_matrix (car cube)) (pc (cdr cube))))
    ))

;; ######## Funciones para rotar columnas ########

;; Funcion para obtener una columna en cierto indice dado
;; @param i: indice de la columna a obtener
;; @param matrix: matriz/cara de donde obtener la columna
(define (get_column i matrix)
    (cond 
        ((zero? i) 
            (col_to_row matrix))
        (else 
            (get_column (- i 1) (delete_col matrix)))))

;; Funcion para obtener todas las columnas laterales, segun la columna seleccionada
;; @param col: indice de la columna a obtener
;; @param cube: Estado del cubo
(define (get_all_columns col cube)
    (get_all_columns_aux 4 col cube))

(define (get_all_columns_aux i col cube)
    (cond 
        ((zero? i) 
            '())
        ((equal? i 4) ; front
            (cons 
                (get_column col (get_face_matrix (actual_face cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 3) ; top
            (cons
                (get_column col (get_face_matrix (get_top cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 2) ; back
            (cons
                (get_column col (get_face_matrix (get_back_of (actual_face cube) cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 1) ; bottom
            (cons
                (get_column col (get_face_matrix (get_bottom cube)))
                (get_all_columns_aux (- i 1) col cube)))
    )
)

;; ((col_W) (col_O) (col_Y) (col_R))

;; Funcion para reemplazar una columna
;; @param col: indice de la columa
;; @param new_col: la columna a escribir
;; @param matrix: cara donde se escribira la nueva columna
(define (set_column col new_col matrix)
    (reverse(rotate_matrix #f
        (set_row col new_col (rotate_matrix #f matrix)))
    )
)

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
;; @param columns: lista de columnas
;; @param flag: determina si el movimiento es hacia arriba o abajo, #t arriba #f abajo 
(define (reverse_columns columns flag)
    (reverse_columns_aux columns flag 0))

(define (reverse_columns_aux columns flag count)
    (cond 
        ((null? columns)
            '())
        ((and flag (equal? count 1)
            (cons
                (reverse (car columns))
                (reverse_columns_aux (cdr columns) flag (+ count 1)))))
        ((equal? count 2)
            (cons 
                (reverse (car columns))
                (reverse_columns_aux (cdr columns) flag (+ count 1))))
        ((and (not flag) (equal? count 3))
            (cons
                (reverse (car columns))
                (reverse_columns_aux (cdr columns) flag (+ count 1))))
        (else 
            (cons 
                (car columns)
                (reverse_columns_aux (cdr columns) flag (+ count 1))))
    ))

;; Funcion para rotar una columna del cubo
;; @param n: tamaño del cubo
;; @param col: índice de la fila a rotar
;; @param down: Boleano que indica si la rotación es hacía arriba o abajo abajo #t arriba #f
;; @param cube: Estado del cubo
(define (rotate_col n col down cube)
    (cond 
        ((zero? col)
            (rotate_face down 0 
                (apply_col_list col 
                    (sort_lists down 
                        (reverse_columns 
                            (get_all_columns 0 cube) (not down))) cube n)))
        ((equal? (- n 1) col)
            (rotate_face (not down) 2 (apply_col_list col (sort_lists down (reverse_columns (get_all_columns 0 cube) (not down))) cube n)))
        (else 
            (apply_col_list col (sort_lists down (reverse_columns (get_all_columns 0 cube) (not down))) cube n))
    ))

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
        ((or (equal? dir "I") (equal? dir "B"))
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
            (rotate_col n index dir cube)) ; Hacer movimiento de columnas
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

(RS 3 cube3x3alt '("C0A"))