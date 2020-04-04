;#lang racket

(define mov '("C0B" "F1I" "C2A"))


;(substring "C1A" 0 1)
;(equal? "C" (substring "C1A" 0 1))
;(string->number "1")

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

;; Función para rotar una fila del cubo.
;; @param n: Tamaño del cubo
;; @param row: Índice de la fila a rotar.
;; @param cw: Boleano que indica si la rotación es en el sentido de las agujas del reloj o al revés.
;; @param cube: Estado del cubo.

(define (rotate_row n row cw cube)
    (cond 
        ((zero? row) 
            (rotate_face cw 4 (apply_rows_list row (sort_lists cw (get_all_rows row cube)) cube)))
        ((equal? (- n 1) row) 
            (rotate_face (not cw) 5 (apply_rows_list row (sort_lists cw (get_all_rows row cube)) cube)))
        (else 
            (apply_rows_list row (sort_lists cw (get_all_rows row cube)) cube))
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

(define (sort_lists cw rows)
    (cond 
        (cw 
            (first_to_last rows))
        (else 
            (last_to_first rows))
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

;; Función para darle vuelta a una lista.
(define (reverse l)
    (cond
        ((null? l) '()) 
        ((append (reverse (cdr l)) (list (car l))))
    ))
(reverse '())

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
 
;; Función para rotar una cara hacia la izquierda o derecha.
;; @param cw: Boleano que indica hacia que lado rotar
;; @param face: Cara del cubo a rotar.
(define (rotate_face cw index cube)
    (cond 
        ((null? cube) 
            '())
        ((zero? index) 
            (cons 
                (append 
                    (list (is_face_selected? (car cube)))
                    (list (rotate_matrix cw (get_face_matrix (car cube)))))
                (rotate_face cw (- index 1) (cdr cube))))
        (else 
            (cons (car cube) (rotate_face cw (- index 1) (cdr cube))))
    ))

;; Función para rotar una matriz hacia la izquierda o derecha.
;; @param cw: Boleano que indica hacia que lado rotar.
;; @param matrix: Matriz a rotar.
(define (rotate_matrix cw matrix)
    (cond 
        ((null? matrix) 
            '())
        (cw 
            (append 
                (list (reverse (col_to_row matrix)))
                (rotate_matrix cw (delete_col matrix))))
        (else 
            (append 
                (rotate_matrix cw (delete_col matrix))
                (list (col_to_row matrix))))
    ))

;; Función que pasa las columnas de una matriz a filas.
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

;(rotate_row 3 2 #f cube)

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
        ((equal? i 4) 
            (cons 
                (get_column col (get_face_matrix(actual_face cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 3)
            (cons
                (get_column col (get_face_matrix(get_top cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 2)
            (cons
                (get_column col (get_face_matrix(get_back_of (actual_face cube) cube)))
                (get_all_columns_aux (- i 1) col cube)))
        ((equal? i 1)
            (cons
                (get_column col (get_face_matrix(get_bottom cube)))
                (get_all_columns_aux (- i 1) col cube)))
    )
)

;(get_all_columns 1 cube)

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




;(rotate_col 1 0 #f cube)
;(rotate_col 3 1 #f cube)
;(rotate_col 3 2 #f cube)
;(rotate_col 3 0 #t cube)
;(quote "case")
;(rotate_col 3 1 #t cube)
;(quote "case")
;(rotate_col 3 2 #t cube)

;(rotate_row 2 0 #t cube2)

#|
    hacia abajo, reverse back_of(3) y bottom(4)
    hacia arriba, reverse back_of(3) y top(2)
|#

;(get_all_columns 0 cube)
;(get_all_columns 1 cube)
;(get_all_columns 2 cube)
;(reverse_columns (get_all_columns 0 cube) #f)
;(reverse_columns (get_all_columns 1 cube) #f)
;(reverse_columns (get_all_columns 2 cube) #f)
;(reverse_columns (get_all_columns 0 cube) #t)

;(sort_lists #f (reverse_columns (get_all_columns 1 cube) #t))
;(sort_lists #t (reverse_columns (get_all_columns 1 cube) #f))

;(apply_col_list 2 (sort_lists #f (reverse_columns (get_all_columns 2 cube) #t)) cube)

;(apply_col_list 0 (last_to_first(reverse_columns (get_all_columns 0 cube) #t)) cube) ;hacia arriba
;(apply_col_list 0 (first_to_last(reverse_columns (get_all_columns 0 cube) #f)) cube) ;hacia abajo


;first_to_last hacia abajo
;last_to_first



;(get_column 0 (get_face_matrix(get_back_of (actual_face cube) cube)))

;(list (set_column 0 (get_column 0 (get_face_matrix(get_bottom cube))) (get_face_matrix (car cube))))




;; Funcion para obtener una lista la secuencia de un movimiento
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

;(get_step_list "C1A")
;(substring "CIA" 2 3)

;; Funcion para obtener la direccion del cubo
;; @param dir: direccion del cubo A
;; filas
;; cw #t izq  y abajo
;; cw #f der y arriba

;; columnas
;; arriba #f
;; abajo #t

;(rotate_row 3 0 #t cube)


(define (get_dir dir)
    (cond 
        ((or (equal? dir "I") (equal? dir "B"))
            #t)
        (else #f)
    ))


;; Funcion para realizar un movimiento segun lo indicado
;; @param f_c: indica si es una fila o columna
;; @param f_c_index: indica el indice de la fila o columna a rotar
;; @param dir: direccion del movimiento
;; @param n: tamaño del cubo
;; @param cube: estado del cubo
(define (make_movement f_c f_c_index dir n cube)
    (cond 
        ((equal? f_c "F") ;Hacer movimiento de filas
            ;(write dir)
            (rotate_row n f_c_index dir cube))
        (else
            ;(write dir)
            (rotate_col n f_c_index dir cube)) ; Hacer movimiento de columnas
    )) 

;; Función principal.
;; @param n: Tamaño del cubo.
;; @param cube: Estado del cubo.
;; @param steps: Movimientos a aplicar al cubo.
(define (RS n cube steps) 
    (cond 
        ((null? steps) cube)
        (else 
            (RS n (make_movement 
                (car(get_step_list (car steps)))
                (string->number(cadr (get_step_list (car steps))))
                (get_dir(caddr (get_step_list (car steps))))
                n cube) (cdr steps))
        )
    ))

;(RS 3 cube mov)
;(get_dir(caddr (get_step_list "F1I")))
;(quote "Caso")
#|
(make_movement "C" 0 "B" 3 
    (make_movement "F" 1 "I" 3 
        (make_movement "C" 2 "A" 3 cube)))


|#


;(rotate_row 3 0 #t cube) ;fila 0 a la izquierda
;(rotate_row 3 1 #t cube) ;fila 1 a la izquierda
;(rotate_row 3 2 #t cube) ;fila 2 a la izquierda

;(rotate_row 3 0 #f cube) ;fila 0 a la derecha
;(rotate_row 3 1 #f cube) ;fila 1 a la derecha
;(rotate_row 3 2 #f cube) ;fila 2 a la derecha

;(rotate_col 3 0 #t cube) ;columna 0 hacia abajo
;(rotate_col 3 1 #t cube) ;columna 1 hacia abajo
;(rotate_col 3 2 #t cube) ;columna 2 hacia abajo

;(rotate_col 3 0 #f cube) ;columna 0 hacia arriba
;(rotate_col 3 1 #f cube) ;columna 1 hacia arriba
;(rotate_col 3 2 #f cube) ;columna 2 hacia arriba

(rotate_col 3 2 #f 
    (rotate_row 3 1 #t  
        ;cube
        (rotate_col 3 0 #t cube)
    )
)



#|
(car(get_step_list (car mov)))
(string->number(cadr (get_step_list (car mov))))
(caddr (get_step_list (car mov)))|#

