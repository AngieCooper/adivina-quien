#lang racket
(require json) ;Librería para abrir los archivos json
(require racket/string) ;Funciones especiales de string

;---------------------------------------------------------------------------------------------
; Objetivo: La clase Personaje tiene como objetivo almacenar la información de cada personaje, así como sus respectivos métodos
; Salida: Un objeto de la clase Personaje
; Entrada:El constructor de la clase recibe un nombre, una lista de caraterísticas y la dirección de la imagen

(define Personaje%
  (class object%
    (init a_nombre)
    (init a_caracteristicas)
    (init a_imagen)
    (super-new)

    (define p_nombre a_nombre)
    (define p_caracteristicas a_caracteristicas)
    (define p_imagen a_imagen)


    ;;SET - GET a_nombre
    (define/public (f_setNombre p_value)
      (set! p_nombre p_value)
    )

    (define/public (f_getNombre)
        p_nombre
    )

    ;;SET - GET a_caracteristicas
    (define/public (f_setCaracteristicas p_value)
      (set! p_caracteristicas p_value)
    )

    (define/public (f_getCaracteristicas)
        p_caracteristicas
    )

    ;;SET - GET a_profesion
    (define/public (f_setImagen p_value)
      (set! p_imagen p_value)
    )

    (define/public (f_getImagen)
        p_imagen
    )

  )
)

;---------------------------------------------------------------------------------------------
; Objetivo: La clase Juego tiene como objetivo almacenar la información que se utilizará para realizar el juego 
; Salida: Un objeto de la clase Juego
; Entrada:El constructor de la clase recibe un objeto Personaje, el personaje del usuario, el personaje de la máquina y el turno

(define Juego%
  (class object%
    (init a_personajes)
    (init a_personajeU)
    (init a_personajeM)
    (init a_turno)
    (super-new)

    (define p_personajes a_personajes)
    (define p_personajeU a_personajeU)
    (define p_personajeM a_personajeM)
    (define p_turno a_turno)
    

    ;;SET - GET a_personajes
    (define/public (f_setPersonajes p_value)
      (set! p_personajes p_value)
    )

    (define/public (f_getPersonajes)
        p_personajes
    )

    ;;SET - GET a_personajeU
    (define/public (f_setPersonajeU p_value)
      (set! p_personajeU p_value)
    )

    (define/public (f_getPersonajeU)
        p_personajeU
    )

    ;;SET - GET a_personajeM
    (define/public (f_setPersonajeM p_value)
      (set! p_personajeM p_value)
    )

    (define/public (f_getPersonajeM)
        p_personajeM
    )

    ;;SET - GET a_turno
    (define/public (f_setTurno p_value)
      (set! p_turno p_value)
    )

    (define/public (f_getTurno)
        p_turno
    )

  )
)


;---------------------------------------------------------------------------------
;Funciones juego

;---------------------------------------------------------------------------------
;Variables globales
(define g_listaPersonajes (list)) ;Lista global con los personajes
(define g_reglas "") ;Lista global con las reglas
(define g_caractConfirmadas (list )) ;Lista global con las caracteristicas confirmadas del personaje a adivinar
(define g_posiblesPersonajes (list )) ;Lista global con las caracteristicas confirmadas del personaje a adivinar
(define g_categorias (list )) ;Lista global con las categorias de las características
(define g_categoriaElegida null) ;Categoria que elige la maquina para hacer la pregunta
(define g_datoElegido null) ;Dato que elige la maquina para hacer la pregunta
(define g_personajeElegido null) ;Se guarda el personaje que adivina la maquina
(define g_juego null) 
; Objetivo: Recibe un hash con atributos del personaje y lo convierte en una lista de lista
; Salida: Lista de listas
; Entrada: Un hash con atributos del personaje

(define (f_hashAList p_hash)
  (define v_caracteristicas (list))
  (define v_llave null)
  (define v_valor null)
  (for ([v_caracteristica p_hash])
    (set! v_llave (symbol->string (car v_caracteristica)))
    (set! v_valor (cdr v_caracteristica))
    (cond
        [(hash? v_valor)(set! v_valor (f_hashAList (hash->list v_valor)))]
     )
    (set! v_caracteristicas (append v_caracteristicas (list(list v_llave v_valor))))
    )
  v_caracteristicas
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Recibe una lista de los personajes con la información en formato hash table y separa cada uno para guardarlo como objeto de la clase y almacenarlos en una sola lista
; Salida: Lista con objetos de la clase Personaje.
; Entrada: La lista con los personajes y su respectiva información en formato hash table

(define (f_guardarPersonajes p_listaPersonajes)
  (define v_listaPersonajes (list ))
  (define v_atributos null)
  (define v_llave null)
  (define v_valor null)
  (define v_nombre null)
  (define v_caract (list))
  (define v_imagen null)
  (for ([v_personaje p_listaPersonajes]) 
    (set! v_atributos (hash->list v_personaje))
    (define v_caracteristicas (list))
    (for ([i v_atributos])
      (set! v_llave (symbol->string (car i)))
      (set! v_valor (cdr i))
      (cond
        [(equal? v_llave "nombre")(set! v_nombre v_valor)]
        [(equal? v_llave "imagen")(set! v_imagen v_valor)]
        [(hash? v_valor)(set! v_valor (f_hashAList (hash->list v_valor)))]
        )
      (cond
        [(and (false? (equal? v_llave "nombre")) (false? (equal? v_llave "imagen")))(set! v_caracteristicas (append v_caracteristicas (list(list v_llave v_valor))))]
        )
      
      )
    (set! v_listaPersonajes (append v_listaPersonajes (list (new Personaje% [a_nombre v_nombre] [a_caracteristicas v_caracteristicas] [a_imagen v_imagen])))) 
    )
  v_listaPersonajes
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Leer un archivo json y obtener la etiqueta personajes
; Salida: Retorna una lista con objetos de la clase Personaje creados con base en la información del json, en caso de no haber personajes retorna una
; una lista vacía
; Entrada: string con la dirección del archivo a leer

(define (f_leerArchivo p_direccion)
  (define jsn (call-with-input-file p_direccion read-json))
  (set! jsn (hash->list jsn))
  (length jsn)
  (for ([objeto jsn])
    (cond
      [(equal? (car objeto) 'personajes) (set! g_listaPersonajes (append g_listaPersonajes (f_guardarPersonajes (cdr objeto))))]
    )
  )
  (set! g_posiblesPersonajes g_listaPersonajes)
  (f_setCategorias)
  (set! g_juego (f_iniciarJuego))
  g_listaPersonajes
  
)

;---------------------------------------------------------------------------------------------
; Objetivo: Unir una categoría padre con una categoría anidada
; Salida: Retorna una lista con la unión de la categoría padre y todas sus categorías anidadas
; Entrada: lista de las categorías anidadas y la categoría padre

(define (f_getCategoriasAnidadas p_lista categoria)
  (define v_resultados (list ))
  (for([anidada p_lista])
    (set! v_resultados (append v_resultados (list (string-append (car anidada) " de " categoria))))
    )
  v_resultados
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene todas la categorías de las características de los personajes
; Salida: Retorna una lista con todas la categorías de las características de los personajes
; Entrada: No tiene

(define (f_setCategorias)
  (define v_caracteristicas (send (car g_listaPersonajes) f_getCaracteristicas))
  (define v_listaCategorias (list ))
  (for([v_caracteristica v_caracteristicas])
    (cond
      [(list? (cadr v_caracteristica)) (set! v_listaCategorias (append v_listaCategorias (f_getCategoriasAnidadas (cadr v_caracteristica) (car v_caracteristica))))]
      [else (set! v_listaCategorias (append v_listaCategorias (list (car v_caracteristica))))]
      )
    )
  (set! g_categorias v_listaCategorias)
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene todas la categorías de las características de los personajes
; Salida: Retorna una lista con todas la categorías de las características de los personajes
; Entrada: No tiene

(define (f_getCategorias)
  g_categorias
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Verifica si un elemento existe en una lista
; Salida: True si existe, False en caso contrario
; Entrada: El elemento que se desea verificar y la lista
(define (existe p_elemento p_lista)
  (define res #f)
  (for ([i p_lista] #:break(equal? res #t))
    (cond [(equal? i p_elemento) (set! res #t)])
    )
  res
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtener la característica de un personaje
; Salida: caracteristica solicitada
; Entrada: El personaje y la caracteristica solicitada

(define (f_getSubCatPersonaje p_categoria p_personaje)
  (define v_caracteristicas (send p_personaje f_getCaracteristicas))
  (define v_res null)
  (for([v_i v_caracteristicas] #:break (false?(null? v_res)))
    (define v_c1 (car v_i))
    (define v_c2  (cadr v_i))
    (cond
      [(equal? v_c1 p_categoria) (set! v_res v_c2)]
      [(string-contains? p_categoria v_c1)
        (for ([v_j v_c2] #:break (false?(null? v_res)))
          (define v_c3 (car v_j))
          (define v_c4  (cadr v_j))
          (cond
          [(string-contains? p_categoria v_c3) (set! v_res v_c4)]
          )
          )
       ]
      )
    )
  v_res
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene todas la categorías de las características de los personajes
; Salida: Retorna una lista con todas la categorías de las características de los personajes
; Entrada: No tiene



;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene todas la categorías de las características de los personajes
; Salida: Retorna una lista con todas la categorías de las características de los personajes
; Entrada: No tiene

(define (f_getAparicionesCategorias)
  (define v_lista (list ))
  (for ([v_cat g_categorias])
    (cond
      [(false?(existe v_cat g_caractConfirmadas)) (set! v_lista (append v_lista (list (list v_cat (f_obtenerApariciones v_cat)))))]
      )
    )
  v_lista
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene todas la categorías de las características de los personajes
; Salida: Retorna una lista con todas la categorías de las características de los personajes
; Entrada: No tiene

(define (f_getAparicionesTodasCategorias)
  (define v_lista (list ))
  (for ([v_cat g_categorias])
    (set! v_lista (append v_lista (list (list v_cat (f_obtenerApariciones v_cat)))))
    )
  v_lista
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene la categoria con menos apariciones
; Salida: Retorna la categoria con menos apariciones
; Entrada: Lista de categorias

(define (f_getCatMenosApariciones p_lista)
  (define v_actual null)
  (define v_nuevaLista (list ))
  (for ([v_i p_lista])
    (define v_temporal null)
    (for ([v_j (cadr v_i)])
      (cond
        [(null? v_temporal) (set! v_temporal v_j)]
        [else (
               cond
                [(< (cadr v_j) (cadr v_temporal)) (set! v_temporal v_j)]
         )]
        )
      )
    (set! v_nuevaLista (append v_nuevaLista (list (list (car v_i) v_temporal))))
    )
  (for ([v_i v_nuevaLista])
    (cond
     [(null? v_actual) (set! v_actual v_i)]
     [else (cond
             [(< (cadadr v_i) (cadadr v_actual)) (set! v_actual v_i)]
            )]
     )
    )
  v_actual
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene la categoria con menos apariciones
; Salida: Retorna la categoria con menos apariciones
; Entrada: Lista de categorias

(define (f_getCatMasApariciones p_lista)
  (define v_actual null)
  (define v_nuevaLista (list ))
  (for ([v_i p_lista])
    (define v_temporal null)
    (for ([v_j (cadr v_i)])
      (cond
        [(null? v_temporal) (set! v_temporal v_j)]
        [else (
               cond
                [(> (cadr v_j) (cadr v_temporal)) (set! v_temporal v_j)]
         )]
        )
      )
    (set! v_nuevaLista (append v_nuevaLista (list (list (car v_i) v_temporal))))
    )
  (for ([v_i v_nuevaLista])
    (cond
     [(null? v_actual) (set! v_actual v_i)]
     [else (cond
             [(> (cadadr v_i) (cadadr v_actual)) (set! v_actual v_i)]
            )]
     )
    )
  v_actual
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene la categoria con menos apariciones
; Salida: Retorna la categoria con menos apariciones
; Entrada: Lista de categorias

(define (f_getMasAparicionesExtra p_lista)
  (define v_actual null)
  (define v_temporal null)
  (for ([v_i p_lista])
      (cond
        [(null? v_temporal) (set! v_temporal v_i)]
        [else (
               cond
                [(> (cadr v_i) (cadr v_temporal)) (set! v_temporal v_i)]
         )]
        )
    )
  v_temporal
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Generar la pregunta que va a hacer computadora
; Salida: Retorna la pregunta formulada
; Entrada: No tiene

(define (f_generarPreguntaM)
  (define v_categoria null)
  (define v_dato null)
  (define v_info null)
  (cond
    [(false? (existe "sexo" g_caractConfirmadas))
      (set! v_info (f_getMasAparicionesExtra (f_obtenerApariciones "sexo")))
      (set! v_categoria "sexo")
      (set! v_dato (car v_info))
      (set! g_caractConfirmadas (append g_caractConfirmadas (list "sexo")))
      ]
    [(> (length g_posiblesPersonajes) (quotient (length g_listaPersonajes) 3))
     
      (set! v_info (f_getCatMasApariciones (f_getAparicionesCategorias)))
      (set! v_categoria (car v_info))
      (set! v_dato (caadr v_info))

      ]
   [(<= (length g_posiblesPersonajes) (quotient (length g_listaPersonajes) 3))
    
      (set! v_info (f_getCatMenosApariciones (f_getAparicionesCategorias)))
      (set! v_categoria (car v_info))
      (set! v_dato (caadr v_info))
      ]
   )
  (set! g_categoriaElegida v_categoria)
  (set! g_datoElegido v_dato)
  (define pregunta (f_formularPregunta v_categoria v_dato))
  
  pregunta
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Formular la pregunta con base en la categoría y el dato recibido
; Salida: Retorna la pregunta formulada
; Entrada: Recibe la categoría de la pregunta y el dato

(define (f_formularPregunta p_categoria p_dato)
  (define v_pregunta (string-append "¿Su " p_categoria " es " p_dato "?"))
  v_pregunta
  )


;---------------------------------------------------------------------------------------------
; Objetivo: Actualizar la lista de apariciones 
; Salida: Retorna la lista de apariciones actualizada
; Entrada: Recibe la lista de apariciones y la nueva aparición de insertar
(define (f_insertarAparicion p_lista p_aparicion)
  (define v_aparece #f)
  (define v_nuevaLista (list ))
  (for ([p_i p_lista])
    (define v_dato (car p_i))
    (define v_aparicion (cadr p_i))
    (cond
      [(equal? v_dato p_aparicion) 
        (set! v_aparicion (+ v_aparicion 1))
        (set! v_aparece #t)
      ]
     )
    (set! v_nuevaLista (append v_nuevaLista (list (list v_dato v_aparicion))))
   )
  (cond
    [(false? v_aparece) (set! v_nuevaLista (append v_nuevaLista (list (list p_aparicion 1))))]
    )
  v_nuevaLista
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Obtiene la cantidad de apariciones de los datos de una categoría
; Salida: Retorna una lista con los datos de la categoría y su cantidad de apariciones
; Entrada: Recibe la categoría

(define (f_obtenerApariciones p_categoria)
  (define apariciones (list ))
  (define v_nuevo null)
  (for([v_personaje g_posiblesPersonajes])
    (define v_caracteristicas (send v_personaje f_getCaracteristicas))
    (for([v_i v_caracteristicas] #:break (false?(null? v_nuevo)))
      (define v_dato1 (car v_i))
      (define v_dato2 (cadr v_i))
      (cond
      [(equal? v_dato1 p_categoria) (set! v_nuevo v_dato2)]
      [(string-contains? p_categoria v_dato1)
        (for ([v_j v_dato2] #:break (false?(null? v_nuevo)))
          (define v_c3 (car v_j))
          (define v_c4  (cadr v_j))
          (cond
          [(string-contains? p_categoria v_c3) (set! v_nuevo v_c4)]
          )
          )
       ]
      )
     )
    (set! apariciones (f_insertarAparicion apariciones v_nuevo))
    (set! v_nuevo null)
   )
  apariciones
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Selecciona dos personajes distintos de manera aleatoria para asignarlos a los jugadores
; Salida: Lista de personajes
; Entrada: No tiene
(define (f_asignarPersonajes)
  (define len (length g_listaPersonajes))
  (define pos (random len))
  (define pos2 (random len))
  (define per1 (list-ref g_listaPersonajes pos))
  (define per2 (list-ref g_listaPersonajes pos2))
  (cond
    [(equal? pos pos2) (f_asignarPersonajes)]
    [else (list per1 per2)]
  )  
)

;---------------------------------------------------------------------------------------------
; Objetivo: Inicia el juego al instanciar la clase Juego%
; Salida: Objeto tipo Juego
; Entrada: No tiene
(define (f_iniciarJuego)
  (define turno (random 2))
  (define personajes (f_asignarPersonajes))
  (define personajeU (list-ref personajes 0))
  (define personajeM (list-ref personajes 1))
  (define juego (new Juego% [a_personajes g_listaPersonajes] [a_personajeU personajeU] [a_personajeM personajeM] [a_turno turno]))
  juego  
)


;---------------------------------------------------------------------------------------------
; Objetivo: Cambia el turno actual
; Salida: Turno actual
; Entrada: Turno anterior
(define (f_cambiarTurno pTurno)
  (cond [(equal? pTurno 1) (set! pTurno 0)] ;Cero corresponde a usuario y 1 a jugador
        [else (set! pTurno 1)])
  pTurno
)

;---------------------------------------------------------------------------------------------
; Objetivo: Recibe las categorías de la pregunta del usuario y compara estas con lo que tiene el personaje máquina
; Salida: 1 sí los datos coincide, null sí no
; Entrada: Categoría principal, característica y sub categoría si lo tiene (por ejemplo en pelo una sub categoría es longitud)
;---------------------------------------------------------------------------------------------
(define (f_evaluaPregunta p_categoria p_caracteristica)
  (define v_caract (send (send g_juego f_getPersonajeM) f_getCaracteristicas))
  (define v_res 0)
  (for ([i v_caract])
    (cond [(equal? (string-downcase (list-ref i 0)) (string-downcase p_categoria))
             (cond[(equal?(string-downcase p_caracteristica) (string-downcase (list-ref i 1))) (set! v_res 1)])]
             [(string-contains? p_categoria (list-ref i 0))
              (for ([i (list-ref i 1)])
                (cond [(string-contains? (string-downcase p_categoria) (string-downcase (list-ref i 0)))
                       (cond[(equal? (string-downcase p_caracteristica) (string-downcase (list-ref i 1))) (set! v_res 1)]
                            )])
                )])
  )
  v_res
)

;---------------------------------------------------------------------------------------------
; Objetivo: Filtra los personajes de acuerdo con la respuesta a la pregunta realizada por la máquina
; Salida: No tiene
; Entrada: La categoría de la caracterítica, la característica y la respuesta en tipo booleano 
;---------------------------------------------------------------------------------------------
(define (f_filtrarPersonajes p_categoria p_dato p_respuesta)
  (define v_personajes (send g_juego f_getPersonajes))
  (define v_personajesCumplen (list))
  (define v_personajesNoCumplen (list))
  (for ([v_perso v_personajes])
    (define v_caracteristicas (send v_perso f_getCaracteristicas))
    (for ([v_caract v_caracteristicas])
      (cond [(equal? (list-ref v_caract 0) p_categoria)
             (cond[(equal? p_dato (list-ref v_caract 1)) (set! v_personajesCumplen (append v_personajesCumplen (list v_perso)))]
                  [else (set! v_personajesNoCumplen (append v_personajesNoCumplen (list v_perso)))])]
            [(string-contains? p_categoria (list-ref v_caract 0))
             (for ([i (list-ref v_caract 1)])
               (cond [(string-contains? p_categoria (list-ref i 0))
                      (cond[(equal? p_dato (list-ref i 1)) (set! v_personajesCumplen (append v_personajesCumplen (list v_perso)))]
                           [else (set! v_personajesNoCumplen (append v_personajesNoCumplen (list v_perso)))])])
             )])
     )
  )
  (cond [(equal? p_respuesta 1) (send g_juego f_setPersonajes v_personajesCumplen) (set! g_posiblesPersonajes v_personajesCumplen) ]
        [else (send g_juego f_setPersonajes v_personajesNoCumplen) (set! g_posiblesPersonajes v_personajesNoCumplen) ])
)

;---------------------------------------------------------------------------------------------
; Objetivo: Crea las reglas que se deben considerar para formular las preguntas de la máquina
; Salida: No tiene
; Entrada: Booleano con la respuesta 1 o 0
;---------------------------------------------------------------------------------------------
(define (f_crearReglas p_respuesta)
  (define v_si (string-append "Su " g_categoriaElegida " si es " g_datoElegido))
  (define v_no (string-append "Su " g_categoriaElegida " no es " g_datoElegido))
  (define v_regla "")
  (cond [(equal? p_respuesta 1) (set! v_regla v_si) (cond [(false?(equal? g_categoriaElegida "sexo")) (set! g_caractConfirmadas (append g_caractConfirmadas (list g_categoriaElegida)))]) ]
        [else (set! v_regla v_no)])
  (set! g_reglas (string-append g_reglas v_regla
                                "\n"))
  
)

(define (f_evaluarRespuesta p_respuesta)
  (define v_respuesta "")
  (f_crearReglas p_respuesta)
  (f_filtrarPersonajes g_categoriaElegida g_datoElegido p_respuesta)
  (cond
    [(equal? (length g_posiblesPersonajes) 1) (set! v_respuesta (f_personajeAdivinado (car g_posiblesPersonajes)))]
    [else
     (cond
       [(equal? (f_anticiparRespuesta) 1) (set! v_respuesta (f_elegirPersonaje))]
       [(equal? (f_anticiparRespuesta) -1) (set! v_respuesta "No existe el personaje") ]
       )]
    )
  
  v_respuesta
  )

(define (f_personajeAdivinado p_personaje)
  (set! g_personajeElegido p_personaje)
  (define v_nombre (send p_personaje f_getNombre))
  (define v_resultado (string-append "Su personaje es " v_nombre))
  v_resultado
  )


(define (f_elegirPersonaje)
  (define v_pos (random (length g_posiblesPersonajes)))
  (define v_personaje (list-ref g_posiblesPersonajes v_pos))
  (define v_respuesta (f_personajeAdivinado v_personaje))
  v_respuesta  
  ) 

;funcion

(define (f_getReglas)
  g_reglas
  )

(define (f_isPersonaje p_nombre)
  (define v_res 0)
  (cond
    [(equal? p_nombre (send (send g_juego f_getPersonajeM) f_getNombre)) (set! v_res 1)]
   )
  
  v_res
  )


;---------------------------------------------------------------------------------------------
; Objetivo: Retorna una lista con la categoría y el mayor numero de apariciones de la característica
; Salida: Lista con la categoria y el mayor numero de apariciones en esa categoria
; Entrada: No tiene
;---------------------------------------------------------------------------------------------
(define (f_mayoresApariciones)
  (define v_todasCategorias (f_getAparicionesTodasCategorias))
  (define v_mayorApariciones (list))
  (for ([v_categoria v_todasCategorias])
    (define v_sublista (list))
    (define v_masAlto 0)
    (for ([v_cont (list-ref v_categoria 1)])
      (cond [(> (list-ref v_cont 1) v_masAlto) (set! v_masAlto (list-ref v_cont 1))])      
    )
    (set! v_sublista (append (list (list-ref v_categoria 0)) (list v_masAlto)))
    (set! v_mayorApariciones (append v_mayorApariciones (list v_sublista)))
  )
  v_mayorApariciones
)

(define (f_getSubCategorias)
  (define v_subCategorias (list ))
  (define v_sub null)
  (for ([p_categoria g_categorias])
    (define v_temp (list ))
    (for([v_personaje g_listaPersonajes])
      (set! v_sub (f_getSubCatPersonaje p_categoria v_personaje))
      (cond
        [(false?(existe v_sub v_temp))(set! v_temp (append v_temp (list v_sub)))]
        )
      )
    (set! v_subCategorias (append v_subCategorias (list v_temp)))
    )
  v_subCategorias
  )

;---------------------------------------------------------------------------------------------
; Objetivo: Promedia la aparición de las características y lo contrasta con una probabilidad para verificar si es prudente anticipar la respuesta
; Salida: Booleano 1 sí se puede anticipar, 0 sí no
; Entrada: No tiene
;---------------------------------------------------------------------------------------------
(define (f_anticiparRespuesta)
  (define v_apariciones (f_mayoresApariciones))
  (define v_lenPersonajes (length (send g_juego f_getPersonajes)))
  (define v_porcentajes (list))
  (define v_res null)
  (cond [(> v_lenPersonajes 0)
        (for ([v_valor v_apariciones])
          (set! v_porcentajes (append v_porcentajes (list (/ (list-ref v_valor 1) v_lenPersonajes))) )
          )
        (define v_sumaPorcentajes 0)
        (for ([v_i v_porcentajes])
          (set! v_sumaPorcentajes (+ v_sumaPorcentajes v_i))
          )
        (set! v_sumaPorcentajes (/ (* v_sumaPorcentajes 100) (length v_apariciones)))
        (cond [(> v_sumaPorcentajes 75) (set! v_res 1)] [else (set! v_res 0)])
  
        ]
        [else (set! v_res -1)]
  )
  v_res
)

(define (f_getInfoPersonaje p_nombre)
  (define v_res "")
  (define v_personaje (f_getPersonajeXNombre p_nombre))
  (define v_sub null)
  (for ([p_categoria g_categorias])
      (set! v_sub (f_getSubCatPersonaje p_categoria v_personaje))
      (set! v_res (string-append v_res p_categoria ": " v_sub "\n"))
      )
  v_res
  )


; Funcion sin comentarios
(define (f_getPersonajeXNombre p_nombre)
  (define v_res null)
  (for ([v_per g_listaPersonajes] #:break (false?(null? v_res)))
      (cond
        [(equal? (send v_per f_getNombre) p_nombre) (set! v_res v_per)]
        )
      )
  v_res
  )

(provide (all-defined-out))