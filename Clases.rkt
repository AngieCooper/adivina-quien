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

;---------------------------------------------------------------------------------
;Funciones juego

(define g_listaPersonajes (list)) ;Lista global con los personajes

;---------------------------------------------------------------------------------------------
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


(f_leerArchivo "C:/Users/snerd/Documents/TEC/Semestre I 2021/Inteligencia Artificial/PY2/Personajes.json")

(provide Personaje%)

