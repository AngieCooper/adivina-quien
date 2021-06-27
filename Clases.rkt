#lang racket
(require json) ;Librería para abrir los archivos json

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

    ;;SET - GET a_nacionalidad
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

(define g_listaPersonajes (list)) ;Lista global con los personajes

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
  ;(print (car(car (send (car g_listaPersonajes) f_getCaracteristicas))))
  (f_getCategorias)
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

(define (f_getCategorias)
  (define v_caracteristicas (send (car g_listaPersonajes) f_getCaracteristicas))
  (define v_listaCategorias (list ))
  (for([v_caracteristica v_caracteristicas])
    (cond
      [(list? (cadr v_caracteristica)) (set! v_listaCategorias (append v_listaCategorias (f_getCategoriasAnidadas (cadr v_caracteristica) (car v_caracteristica))))]
      [else (set! v_listaCategorias (append v_listaCategorias (list (car v_caracteristica))))]
      )
    )
  (print v_listaCategorias)
  v_listaCategorias
  )

(f_leerArchivo "Personajes.json")

;---------------------------------------------------------------------------------------------
; Objetivo: Selecciona dos personajes distintos de manera aleatoria para asignarlos a los jugadores
; Salida: Lista de personajes
; Entrada: No tiene
(define (f_asignarPersonajes)
  (define len (length g_listaPersonajes))
  (print len)
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
(define (evalua p_categoria p_caracteristica p_subcategoria)
  (define v_caracts (send (send g_juego f_getPersonajeM) f_getCaracteristicas))
  (for ([i v_caracts])
    (cond [((listof string?) i)
           (cond [(equal? p_categoria (list-ref i 0))
                  (cond[(equal? p_caracteristica (list-ref i 1)) (print 1)])]
                  )]
        [else
           (cond [(equal? p_categoria (list-ref i 0))
                  (for ([j (list-ref i 1)])
                    (cond [(equal? p_subcategoria (list-ref j 0))
                           (cond [(equal? p_caracteristica (list-ref j 1)) (print 1)]) ])
                  )])
        ])

  )
)
;(evalua "etnicidad" "afro" "")
(define g_juego (f_iniciarJuego)) ;Lista global con los personajes
 
(provide Personaje%)
(provide Juego%)
;(evaluarPregunta "sexo" "color" "Masculino")     (list-ref (list-ref caracts 1) 0)
