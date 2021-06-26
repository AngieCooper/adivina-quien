#lang racket


(require racket/gui)

;Ruta del json
;json path object

(define jsonClass%
  (class object%
  (define ruta "None")
  (super-new)

  (define/public (get-ruta) ruta)
  (define/public (set-ruta new-ruta) (set! ruta new-ruta))
  ))

;;///////////////////////////////////////////////////////////////////////
;;///////////////////////////////////////////////////////////////////////
;;///////////////////////////////////////////////////////////////////////
(define miPrincipal (new frame%
                         [label "Adivina quien"]
                         ))


;;principal panel
#|

    --------------------------------------------
    |                                          |
    |                                          |
    |                                          |
    |                                          |
    |                                          |
    |                                          |
    |                                          |
    |                                          |
    --------------------------------------------

|#

(define panelHorizontal1 (new horizontal-panel%
                              [parent miPrincipal]
                              [style '(border)]
                              [alignment '(center center)]
                              [vert-margin 25]
                              [horiz-margin 25]
                              [spacing 5]
                              [border 25]))


;-----------------------------Preguntas Panel-----------------------------------------
(define pregVerticalPnl (new vertical-panel%
                              [parent panelHorizontal1]
                              [style '(border)]
                              [alignment '(center center)]
                              [vert-margin 5]
                              [horiz-margin 5]
                              [spacing 15]
                              [border 25]))

(define form-preg-lbl (new message%
                           [parent pregVerticalPnl]
                           [label "Formulacion de preguntas"]
                           [font normal-control-font]
                           [auto-resize #t]
                           [min-width 500]
                           [min-height 500]))

(define listCat (new combo-field%
                     [parent pregVerticalPnl]
                     [label "Categorias"]
                     [choices '("Categoria 1" "Categoria 2" "Categoria 2")]))

(define listCatOpc (new combo-field%
                     [parent pregVerticalPnl]
                     [label "Opciones"]
                     [choices '("Opcion 1" "Opcion 2" "Opcion 3")]))

(define pregForm (new text-field%
                      [parent pregVerticalPnl]
                      [label "Pregunta realizada"]
                      [enabled #f]))

;-----------------
(define buttonACPnl (new horizontal-panel%
                              [parent pregVerticalPnl]
                              [alignment '(center center)]      
                              ))
(define button-preguntar (new button%
                         [parent buttonACPnl]
                         [label "Preguntar"]))

(define button-Limpiar (new button%
                         [parent buttonACPnl]
                         [label "Limpiar"]))
;--------------

;----------------------------------Chat panel------------------------------------
(define chatVerticalPnl (new vertical-panel%
                              [parent panelHorizontal1]
                              [style '(border)]
                              [alignment '(center center)]
                              [vert-margin 5]
                              [horiz-margin 5]
                              [spacing 25]
                              [border 25]))

(define chat-lbl (new message%
                           [parent chatVerticalPnl]
                           [label "Historial de preguntas"]
                           [font normal-control-font]
                           [auto-resize #t]
                           [min-width 100]
                           [min-height 100]))

(define chatPnl (new vertical-panel%
                              [parent chatVerticalPnl]
                              [style '(border auto-vscroll )]
                              [alignment '(center center)]
                              [vert-margin 1]
                              [horiz-margin 1]
                              [spacing 2]
                              [border 5]))

(define buttonYNPnl (new horizontal-panel%
                              [parent chatVerticalPnl]
                              [alignment '(center center)]
                              [style '(border)]
                              ))
(define yes-button (new button%
                         [parent buttonYNPnl]
                         [label "Si"]))

(define No-button (new button%
                         [parent buttonYNPnl]
                         [label "No"]))

;--------------------------------Descision Panel -----------------------------
(define descVerticalPnl (new vertical-panel%
                              [parent panelHorizontal1]
                              [style '(border)]
                              [alignment '(center center)]
                              [vert-margin 5]
                              [horiz-margin 5]
                              [spacing 25]
                              [border 25]))

(define choices-lbl (new message%
                           [parent descVerticalPnl]
                           [label "Decisiones maquina"]
                           [font normal-control-font]
                           [auto-resize #t]
                           [min-width 500]
                           [min-height 500]))

(define choicesMchPnl (new vertical-panel%
                              [parent descVerticalPnl]
                              [style '(border auto-vscroll )]
                              [alignment '(center center)]
                              [vert-margin 5]
                              [horiz-margin 5]
                              [spacing 2]
                              [border 5]))

;------------------------------Character Panel --------------------------------
(define charVerticalPnl (new vertical-panel%
                              [parent miPrincipal]
                              [style '(border)]
                              [alignment '(left top)]
                              [horiz-margin 5]
                              [vert-margin 5]
                              [spacing 5]
                              [border 25]))

(define characters-lbl (new message%
                           [parent charVerticalPnl]
                           [label "Personajes"]
                           [font normal-control-font]
                           [auto-resize #t]
                           [min-width 500]
                           [min-height 500]))

;; ///////////////////////////////////////////////////////////////////
;;////////////////////////////////////////////////////////////////////

(define jsonObj (new jsonClass%))
(define frameInit (new frame% [label "Inicio"]))

(define panelInit (new vertical-panel%
                       [parent frameInit]
                       [style '(border)]
                       [alignment '(center center)]
                       [vert-margin 25]
                       [horiz-margin 25]
                       [spacing 25]
                       [border 25]))

(define msg (new message% [parent panelInit]
                 [label "¡Adivina quien!"]
                 [font normal-control-font]
                 [auto-resize #t]
                 [min-width 500]
                 [min-height 500]))

(define box-in (new text-field%
                      [parent panelInit]
                      [label "Json"]
                      [enabled #f]))

(define panel-button (new horizontal-panel%
                          [parent panelInit]))

(define button-open (new button%
                         [parent panel-button]
                         [label "Abrir"]
                         [callback (lambda(button event)
                                     (let ([ruta (send jsonObj get-ruta)])
                                       (if (path? ruta)
                                           (begin
                                             (writeln "\n Exito al abrir el archivo")
                                             (send miPrincipal show #t))
                                           (writeln "indefinido"))))]))

(define button-search (new button%
                         [parent panel-button]
                         [label "Buscar"]
                         [callback (lambda(button event)
                                    (let ([ruta (get-file)])
                                       (if (path? ruta)
                                           (begin                                             
                                             (send jsonObj set-ruta ruta)
                                             (send box-in set-value (path->string ruta)))
                                           (begin
                                             (send jsonObj set-ruta "None")
                                             (send box-in set-value "indefinido"))
                                     )))]))

;;///////////////////////////////////////////////////////////////////////
;;///////////////////////////////////////////////////////////////////////
; Show the dialog
(send miPrincipal show #t)