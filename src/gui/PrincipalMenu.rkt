#lang racket/gui

;Frame principal donde se colocaran botones para seleccionar el modo
(define principalFrame(new frame% 
    [label "Menu principal"]
    [width 500]
    [height 300]))


;Panel vertical para colocar botones
(define panelH (new vertical-panel% 
    [parent principalFrame]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define welcomeMessage (new message% 
    [parent panelH]
    [label "Bienvenido a Rubik Simulator!"]
    [font (make-object font% 25 'default 'normal 'bold)]))

(define Secuencialbutton (new button%
    [parent panelH]
    [min-width 150]
    [min-height 100]
    [font (make-object font% 25 'default 'normal 'bold)]
    [label "Modo Secuencial"]
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
            (send ManualFrame show #t)
            (send principalFrame show #f))]))


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
    [label "Modo Secuencial"]))

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
    [min-value 1]
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

(define initSecuencialButton (new button%
    [parent secuencialPanel]
    ;[min-width 150]
    ;[min-height 100]
    ;[font (make-object font% 25 'default 'normal 'bold)]
    [label "Iniciar"]
    [callback (lambda (button event)
            (send secuencialFrame show #f)
            )]))



;Vista de modo manual
(define ManualFrame (new frame%
    [label "Modo Manual"]
    [width 700]
    [height 500]))




(send principalFrame show #t)