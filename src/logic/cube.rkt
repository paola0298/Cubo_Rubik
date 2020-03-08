#lang racket

;; Cube face
;; (selected (row1 row2 ... rowN))
;; (#t (("B" "B" "B") ("B" "B" "B") ("B" "B" "B")))
;; 
;; Cube state
;; (face1 face2 ... face6)

(define cube 
    '(
        (#T (("W" "W" "W")
             ("W" "W" "W")
             ("W" "W" "W")))
        (#T (("B" "B" "B")
             ("B" "B" "B")
             ("B" "B" "B")))
        (#T (("Y" "Y" "Y")
             ("Y" "Y" "Y")
             ("Y" "Y" "Y")))
        (#T (("G" "G" "G")
             ("G" "G" "G")
             ("G" "G" "G")))
        (#T (("P" "P" "P")
             ("P" "P" "P")
             ("P" "P" "P")))
        (#T (("O" "O" "O")
             ("O" "O" "O")
             ("O" "O" "O")))
    ))

;; (RS tamaño_cubo cubo movimientos)



(define (RS n cube steps) 
    (cond 
        ((zero? n) #F)
        (else #T)
        ))

(RS 1 '() '())

