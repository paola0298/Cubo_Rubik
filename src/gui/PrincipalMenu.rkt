#lang racket/gui

(define principalFrame(new frame% 
    [label "Menu principal"]
    [width 500]
    [height 300]))

(define panelH (new vertical-panel% 
    [parent principalFrame]
    [horiz-margin 15]
    [vert-margin 15]
    [spacing 10]))

(define secuencialFrame (new frame%
    [label "Modo secuencial"]
    [width 700]
    [height 500]))

(define ManualFrame (new frame%
    [label "Modo Manual"]
    [width 700]
    [height 500]))

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


(send principalFrame show #t)