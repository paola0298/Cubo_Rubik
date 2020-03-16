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
(define (actual_face cube) 
    (cond 
        ((caar cube) 
            (car cube))
        (else 
            (actual_face (cdr cube)))
    ))


;; Función para obtener la cara superior del cubo.
;; @param cube: Estado del cubo.
(define (get_top cube)
    (cond 
        ((null? (cddr cube))
            (car cube))
        (else 
            (get_top (cdr cube)))
    ))

;; Función para verificar si una cara dada es la superior.
;; @param face: Cara a verificar.
;; @param cube: Estado del cubo.
(define (is_top? face cube)
    (equal? face (get_top cube)))

;; Función para obtener la cara inferior del cubo.
;; @param cube: Estado del cubo.
(define (get_bottom cube)
    (cond 
        ((null? (cdr cube))
            (car cube))
        (else 
            (get_bottom (cdr cube)))
    ))

;; Función para verificar si una cara dada es la inferior.
;; @param face: Cara a verificar.
;; @param cube: Estado del cubo.
(define (is_bottom? face cube)
    (equal? face (get_bottom cube)))


;; Función para obtener el índice de una cara.
;; @param face: Cara a obtener su índice.
;; @param cube: Estado del cubo.
(define (get_face_index face cube)
    (cond 
        ((equal? (car cube) face)
            0)
        (else 
            (+ 1 (get_face_index face (cdr cube))))
    ))

;; Función para obtener la cara en un índice dado.
;; @param index: Índice del cual buscar la cara.
;; @param cube: Estado del cubo.
(define (get_face_at index cube)
    (cond 
        ((zero? index) 
            (car cube))
        (else 
            (get_face_at (- index 1) (cdr cube)))
    ))

(define (set_face_at index face cube)
    (#F))

;; Función para obtener la cara opuesta a otra.
;; @param face: Cara de la cual obtener su opuesta.
;; @param cube: Estado del cubo.
(define (get_back_of face cube)
    (cond 
        ((is_top? face cube)
            (get_bottom cube))
        ((is_bottom? face cube)
            (get_top cube))
        (else
            (get_face_at (remainder (+ (get_face_index face cube ) 2) 4) cube))
    ))

(define (is_back_of? face back)
    (equal? back (get_back_of face)))

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
                (list (is_face_selected? (car cube)))
                (list (set_column col (cadr columns) (get_face_matrix (car cube))))))
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




;(rotate_col 3 0 #f cube)
;(rotate_col 3 1 #f cube)
;(rotate_col 3 2 #f cube)
(rotate_col 3 0 #t cube)
(quote "\n case \n")
(rotate_col 3 1 #t cube)
(quote "case \n")
(rotate_col 3 2 #t cube)

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


