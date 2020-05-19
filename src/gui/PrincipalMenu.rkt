#lang racket/gui

;; Se importan las librerías necesarias.
(require 
    racket/include
    pict3d
    pict3d/universe)

;; Se importan estados de cubo de ejemplo.
(include "../logic/cube_state.rkt")

;; Se importan las funciones básicas de rotación
(include "../logic/basic_functions.rkt")
(include "../logic/rotation_functions.rkt")

;; Se importan las funciones lógicas principales
(include "../logic/main_functions.rkt")

;; Se importa la configuración de las escenas.
(include "general_config.rkt")
(include "secuential_config.rkt")
(include "interactive_config.rkt")

;; Se importan las funciones de renderizado.
(include "render_functions.rkt")

;; Se importa el modo secuencial.
(include "secuential.rkt")
;; Se importa el modo interactivo.
(include "interactive.rkt")

;; Función para convertir un string de una lista a una lista.
;; @param str: String a convertir a lista.
(define (convert str)
    (with-input-from-string str read))

;; Función para verificar si un string es numérico.
;; @param s: String a verificar.
(define (string-numeric? s) 
    (string->number s))

;Frame principal donde se colocaran botones para seleccionar el modo
(define principalFrame(new frame% 
    [label "Menu principal"]
    [width 500]
    [height 300]))


;Panel vertical para colocar botones
(define panelH (new vertical-panel% 
    [parent principalFrame]
    [horiz-margin 15]
    [vert-margin 20]
    [spacing 40]))

(define welcomeMessage (new message% 
    [parent panelH]
    [label "Bienvenido a Rubik Simulator!"]
    [font (make-object font% 25 'default 'normal 'bold)]))

;; TODO: renombrar a secuencial cuando el modo interactivo esté listo
(define Secuencialbutton (new button%
    [parent panelH]
    [min-width 150]
    [min-height 100]
    [font (make-object font% 25 'default 'normal 'bold)]
    [label "Modo secuencial"]
    [callback (lambda (button event)
            (send secuencialFrame show #t)
            (send principalFrame show #f))]))


(define Manualbutton (new button%
    [parent panelH]
    [min-width 150]
    [min-height 100]
    [font (make-object font% 25 'default 'normal 'bold)]
    [label "Modo Manual"]
    [callback (lambda (button event)
            ;(send ManualFrame show #t)
            (send principalFrame show #f)
            (set! cube-size 3)
            (set! cube-internal-state cube3x3)
            (big-bang3d empty-pict3d
                #:name "Rubik's Simulator - Interactivo"
                ;#:display-mode 'fullscreen
                #:frame-delay (/ 1000 60)
                #:on-frame on-frame-interactive
                #:on-key on-key-interactive
                #:on-draw on-draw-interactive)
            )]))


;Vista de menu de modo secuencial
;Frame donde iran todos los widgets
(define secuencialFrame (new frame%
    [label "Modo secuencial"]
    [width 700]
    [height 500]))

(define secuencialPanel (new vertical-panel%
    [parent secuencialFrame]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define welcomeSecuencialMessage (new message% 
    [parent secuencialPanel]
    [label "Modo Secuencial"]
    [font (make-object font% 17 'default 'normal 'bold)]))

(define settingsPanel(new vertical-panel%
    [parent secuencialPanel]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define sizePanel (new vertical-panel%
    [parent settingsPanel]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define initialStatePanel (new vertical-panel%
    [parent settingsPanel]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define movementsPanel (new vertical-panel%
    [parent settingsPanel]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))


(define sizeMessage (new message%
    [parent sizePanel]
    [label "Tamaño del cubo"]))

(define initialStateMessage (new message%
    [parent initialStatePanel]
    [label "Estado inicial del cubo"]))

(define movementsMessage (new message%
    [parent movementsPanel]
    [label "Movimientos a realizar"]))

(define sizeSlider (new slider%
    [parent sizePanel]
    [label ""]
    [min-value 2]
    [max-value 15]
    [init-value 3]))

(define initialTextField (new text-field%
    [parent initialStatePanel]
    [label ""]
    [init-value ""]
    ;[min-width 150]
    [min-height 50]
    [style '(multiple)]))

(define movementsTextField (new text-field%
    [parent movementsPanel]
    [label ""]
    [init-value ""]
    ;[min-width 150]
    [min-height 50]
    [style '(multiple)]))

(define (setParameters size initial movements)
    (printf "Size ~a.\n" size)
    (printf "Initial state of cube ~a.\n" initial)
    (printf "Movements ~a.\n" movements)
    (set! cube-size size)
    (set! cube-internal-state (convert initial))
    (set! cube-steps (convert movements)))

(define (initSecuencialCallback b e)
    (let ((size (send sizeSlider get-value))
          (initialCube (send initialTextField get-value))
          (movements (send movementsTextField get-value)))
    (cond ((and (equal? initialCube "") (equal? movements "")
        (message-box "Error" "Por favor complete todos los campos" secuencialFrame '(stop ok))))
        (else 
            (send secuencialFrame show #f)
            (setParameters size initialCube movements)
            (big-bang3d empty-pict3d
                #:name "Rubik's Simulator - Secuencial"
                #:display-mode 'fullscreen
                #:frame-delay (/ 1000 60)
                #:on-frame on-frame-secuential
                #:on-draw on-draw-secuential)
            ))))


(define initSecuencialButton (new button%
    [parent secuencialPanel]
    ;[min-width 150]
    ;[min-height 100]
    [font (make-object font% 15 'default 'normal 'bold)]
    [label "Iniciar"]
    [callback initSecuencialCallback]))



;Vista de modo manual
(define ManualFrame (new frame%
    [label "Modo Manual"]
    [width 700]
    [height 500]))

(send principalFrame show #t)
