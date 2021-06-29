#lang racket


(require racket/gui)
(require "Clases.rkt")

(define caf null)
(define maquina 0)
(define usuario 1)


;Ruta del json
;json path object

(define jsonClass%
  (class object%
  (define ruta "None")
  (super-new)

  (define/public (get-ruta) ruta)
  (define/public (set-ruta new-ruta) (set! ruta new-ruta))
  ))

(define negro (make-object color% 0 0 0))
;;///////////////////////////////////////////////////////////////////////
;;///////////////////////////////////////////////////////////////////////
;;///////////////////////////////////////////////////////////////////////
(define miPrincipal (new frame%
                         [label "Adivina quien"]
                         [width 1200]
                         [height 720]
                         [x 0]
                         [y 0]
                         [style '(fullscreen-button
                                  no-resize-border
                                  toolbar-button
                                  metal)]
                         ))

(define turnolbl (new message%
                      [parent miPrincipal]
                      [label "Es el turno de: "]))
;;principal panel
(define panelHorizontal1 (new horizontal-panel%
                              [parent miPrincipal]
                              [style '(border)]
                              [alignment '(center center)]
                              [vert-margin 25]
                              [horiz-margin 25]
                              [spacing 5]
                              [border 25]))


;-----------------------------Preguntas Panel-----------------------------------------

(define funcionSN (lambda(num)
                    (cond ((= num 0) (new message%
                                          [parent chatPnl]
                                          [label "Jugador Automático: No"]))
                          (else (new message%
                                     [parent chatPnl]
                                     [label "Jugador Automático: Si"])))))

(define categorias '("Cat1" "Cat2" "Cat3"))

(define subCat '(("c1" "c2" "c3") ("a1" "a2" "a3") ("b1" "b2" "b3")))

(define getSubCat (lambda(cat)
                    (let ([cats categorias]
                          [subc subCat])
                    (letrec ([fun (lambda(cat categorias subCat)                                    
                                    (cond ((string=? cat (car categorias)) (car subCat))
                                          (else (fun cat (cdr categorias) (cdr subCat)))))])
                      (fun cat cats subc)))))

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
                           [auto-resize #t]))

(define lista-Cat
  (new (class combo-field%      
         (super-new)     
         (inherit get-menu append)
         (define/public (update-choices choice-list)
           ; remove all the old items
           (map
             (lambda (i)
               (send i delete))
             (send (get-menu) get-items))
           ; set up the menu with all new items
           (map
            (lambda (choice-label)
              (append choice-label))
            choice-list)
           (void))
         )
       [callback (lambda(button event)
                   (let ([subs (getSubCat (send button get-value))])                     
                     (send combo-field update-choices subs)))]
       [parent pregVerticalPnl]
       [label "Categorias"]
       [choices '()]
       [stretchable-width #t]
       [stretchable-height #f]
       ))


(define combo-field
  (new (class combo-field%      
         (super-new)     
         (inherit get-menu append)
         (define/public (update-choices choice-list)
           ; remove all the old items
           (map
             (lambda (i)
               (send i delete))
             (send (get-menu) get-items))
           ; set up the menu with all new items
           (map
            (lambda (choice-label)
              (append choice-label))
            choice-list)
           (void))
         )
       
       [parent pregVerticalPnl]
       [label "SubCategorias"]
       [callback (lambda (button event)
                   (let* ([subc (send button get-value)]
                          [cat (send lista-Cat get-value)]
                          [pregunta (f_formularPregunta cat subc)])
                      (send pregForm set-value pregunta)))]
       [choices '()]
       [stretchable-width #t]
       [stretchable-height #f]
       ))


(define pregForm (new text-field%
                      [parent pregVerticalPnl]
                      [label "Pregunta realizada"]
                      [enabled #f]))

;-----------------
(define buttonACPnl (new horizontal-panel%
                              [parent pregVerticalPnl]
                              [alignment '(center center)]      
                              ))

(define funcion-turno (lambda()
                        (let ([turno (send g_juego f_getTurno)])
                        (cond ((= turno maquina) "Jugador Automatico")
                              (else "Usuario")))))
(define button-preguntar (new button%
                         [parent buttonACPnl]
                         [label "Preguntar"]
                         [callback (lambda(button event)               
                                     (let([pregunta (send pregForm get-value)]
                                          [cat (send lista-Cat get-value)]
                                          [subc (send combo-field get-value)])
                                       (cond((not(string=? "" pregunta))
                                             (new message% [parent chatPnl]
                                                  [label (string-append "Yo: " pregunta)])
                                              (funcionSN (f_evaluaPregunta cat subc))
                                              (send g_juego f_setTurno (f_cambiarTurno (send g_juego f_getTurno)))
                                              (send turnolbl set-label (string-append "Turno de " (funcion-turno)))
                                              (jugar))
                                            ((println "Pregunta vacia"))))
                                     (send pregForm set-value ""))]))

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

(define ganar-funcion (lambda(jugador)
                        (let*([dialg (instantiate dialog% ("Ganador"))]
                              [msg (new message% [parent dialg]
                                        [label (string-append "Ha ganado: " jugador)])])
                          (send framePersonajes show #f)
                          (send dialg show #t))))
(define yes-button (new button%
                         [parent buttonYNPnl]
                         [label "Si"]
                         [callback (lambda(button event)
                                     (new message%
                                          [parent chatPnl]
                                          [label "Yo: si"])
                                      (let ([res (f_evaluarRespuesta 1)])
                                        (send labelCh set-label (f_getReglas))
                                        (cond ((not(string=? res ""))
                                               ((new message%
                                                     [parent chatPnl]
                                                     [label res])))
                                              ((not (null? g_personajeElegido))
                                               (ganar-funcion "Jugador Automático"))
                                              (else
                                               (send g_juego f_setTurno (f_cambiarTurno (send g_juego f_getTurno)))
                                                (send turnolbl set-label (string-append "Turno de: " (funcion-turno)))
                                                (jugar))))
                                      )]))

(define no-button (new button%
                         [parent buttonYNPnl]
                         [label "No" ]
                         [callback (lambda(button event)
                                     (new message%
                                          [parent chatPnl]
                                          [label "Yo: No"])
                                      (let ([res (f_evaluarRespuesta 0)])
                                        (send labelCh set-label (f_getReglas))
                                        (cond ((not(string=? res ""))
                                               ((new message%
                                                     [parent chatPnl]
                                                     [label res]))
                                               (cond ((not (null? g_personajeElegido))
                                               (ganar-funcion "Jugador"))))
                                              (else (send g_juego f_setTurno (f_cambiarTurno (send g_juego f_getTurno)))
                                                     (send turnolbl set-label (string-append "Turno de " (funcion-turno)))
                                                     (jugar)))))]))

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
                              [alignment '(left top)]
                              [vert-margin 5]
                              [horiz-margin 5]
                              [spacing 2]
                              [border 5]))

(define labelCh (new message% [parent choicesMchPnl] [label ""]))

;------------------------------Character Frame --------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
(define framePersonajes (new frame%
                   [label "Personajes"]
                   ;[width 300]
                   [height 500]
                   ))

;Panel para contener las imagenes de personajes
(define frame (new vertical-panel%
                   [parent framePersonajes]
                   [style '(auto-vscroll)]))

;Funcion para obtener una imagen dado su nombre en string
; string -> bitmap%
(define obtener-imagen (lambda(nombre)
                         (make-object bitmap% nombre)))
;Funcion para tener la imagen de descartado
;() -> bitmap%
(define descartado (obtener-imagen "descartado.png"))


(define grilla '(0))

; Funcion que cumple la tarea de descartar un personaje y tachar su nombre
(define (descartar-f button-char button-elegir)
  (lambda(nombre)
    (send button-char set-label descartado)
    (println (string-append "Se ha descartado " nombre))
    (send button-elegir enable #f)))




;Funcion que sirve para mostrar la info del personaje
(define crear-msg (lambda(personaje) (let* ([dialogo (instantiate dialog% ("Informacion"))]
                                            [info_p (f_getInfoPersonaje personaje)]
                                            [info (new text-field%
                                                       [parent dialogo]
                                                       [label #f]
                                                       [style '(multiple)]
                                                       [min-width 200]
                                                       [min-height 200]
                                                       [enabled #f])])
                                       (send info set-value info_p)
                                       (send dialogo show #t) )))

;Descarta al personaje y lo vuelve inelegible
(define (dis-f entrada button-img btn-choice)
  (send button-img enable #f)
  (send btn-choice enable #f))

; crea la caja del personaje con los botones de elegir y descarte
(define crear-caja (lambda(padre imagen personaje)
                     (let* ([caja (new vertical-panel% [parent padre] [style '(border)] [alignment '(left center)])]
                            [nombre (new message% [parent caja] [label personaje])]
                            [btnImg (new button% [parent caja]
                                        [label imagen]
                                        [callback (lambda(button event) ( crear-msg personaje))])]
                            [cajaBtn (new horizontal-panel% [parent caja])]
                            [chooseBtn (new button%
                                            [parent cajaBtn]
                                            [label "Escoger"]
                                            [callback (lambda (button event)
                                                        (new message%
                                                             [parent chatPnl]
                                                             [label (string-append "Yo: Tu personaje es " personaje "?")])
                                                         (let*([resp (f_isPersonaje personaje)])
                                                                           (cond ((= resp 1) (ganar-funcion "Jugador"))
                                                                                 (else (ganar-funcion "Jugador automático"))))
                                                         )])]
                            [dropBtn (new button% [parent cajaBtn]
                                         [label "Descartar"]
                                         [callback (lambda(button event)
                                                     (dis-f "Hola" btnImg chooseBtn)
                                                     (send button enable #f))])])
                       caja)))

;crea un nuevo contenedor donde se almacenaran los personajes en filas
;() -> vertical-panel%
(define container (new vertical-panel% [parent frame]))

;crea las filas y agrega a los personajes
(define agregar-grilla (lambda(contador personajes grilla padre)
                         (cond ((null? personajes) '())
                               ((= (modulo contador 5) 0)
                                (let* ([nueva-fila (new horizontal-panel% [parent padre])]
                                       [persona (car personajes)]
                                       [nombre_p (send persona f_getNombre)]
                                       [img (send persona f_getImagen)]
                                       [caja (crear-caja nueva-fila (obtener-imagen img ) nombre_p)])
                                  (agregar-grilla (+ contador 1) (cdr personajes) nueva-fila padre)))
                               (else
                                (let* (      
                                       [persona (car personajes)]
                                       [nombre_p (send persona f_getNombre)]
                                       [img (send persona f_getImagen)]
                                       [caja (crear-caja grilla (obtener-imagen img) nombre_p)])
                                (agregar-grilla (+ contador 1) (cdr personajes) grilla padre))))))


;

;zona de trabajo

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

(define funcion-info (lambda(persona)
                       (let* ([dialog (instantiate dialog% ("Info"))]
                              [mess (new message%
                                         [parent dialog]
                                         [label (string-append "Tu personaje es: " persona)])]
                              [Img (obtener-imagen (send (send g_juego f_getPersonajeU) f_getImagen))]
                              [btnImg (new button%
                                           [parent dialog]
                                           [label Img])])
                         (send dialog show #t))))

(define funcion-gane (lambda(persona)
                       (let* ([dialog (instantiate dialog% ("Ganar"))]
                              [mess (new message%
                                         [parent dialog]
                                         [label (string-append "Tu has " persona)])]
                              )
                         #f)))

(define button-open (new button%
                         [parent panel-button]
                         [label "Abrir"]
                         [callback (lambda(button event)
                                     (let ([ruta (send jsonObj get-ruta)])
                                       (if (path? ruta)
                                           (begin
                                             (set! caf (f_leerArchivo (path->string ruta)))
                                             (set! subCat (f_getSubCategorias))
                                             (set! categorias (f_getCategorias))
                                             (send lista-Cat update-choices (f_getCategorias))
                                             (agregar-grilla 0 caf null frame )
                                             (send miPrincipal show #t)
                                             (send frameInit show #f)
                                             ;(display (send (send g_juego f_getPersonajeM) f_getNombre))
                                             (funcion-info (send (send g_juego f_getPersonajeU) f_getNombre))
                                             (send turnolbl set-label (string-append "Turno de " (funcion-turno)))
                                             (jugar))
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

(new button%
     [parent miPrincipal]
     [label "Muestra personajes"]
     [callback (lambda (button event)
                 (send framePersonajes show #t))])

(send frameInit show #t)
(send miPrincipal show #f)
(send framePersonajes show #f)

(define jugar (lambda()
                (cond ((= (send g_juego f_getTurno) maquina)
                       (send lista-Cat enable #f)
                       (send combo-field enable #f)
                       (send button-Limpiar enable #f )
                       (send button-preguntar enable #f )
                       (send yes-button enable #t)
                       (send no-button enable #t)
                       (new message% [parent chatPnl]
                            [label (string-append "Jugador Automático: " (f_generarPreguntaM))]))
                      
                      (else
                       (send yes-button enable #f)
                        (send no-button enable #f)
                        (send lista-Cat enable #t)
                        (send combo-field enable #t)
                        (send button-Limpiar enable #t)
                        (send button-preguntar enable #t)))))
