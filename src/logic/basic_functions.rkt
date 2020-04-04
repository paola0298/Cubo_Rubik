;; Función para obtener la cara seleccionada.
;; @param cube: Estado del cubo. 
(define (actual_face cube) 
    (cond 
        ((caar cube) 
            (car cube))
        (else 
            (actual_face (cdr cube)))
    ))

;; Función para verificar si la cara dada está seleccionada.
;; @param face: Cara del cubo a verificar.
(define (is_face_selected? face)
    (car face))

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

;; Función para verificar si una cara es la opuesta a otra.
;; @param face: Cara del frente.
;; @param back: Cara a verificar si es opuesta a face.
;; @param cube: Estado del cubo.
(define (is_back_of? face back cube)
    (equal? back (get_back_of face cube)))

;; Función para devolver la matriz de la cara dada.
;; @param face: Cara de donde obtener la matriz.
(define (get_face_matrix face)
    (cadr face))
