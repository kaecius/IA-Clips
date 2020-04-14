;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pont")
;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pins")


(defglobal
    ?*TIEMPO-RECARGA* = (* -1 5)
    ?*DECREMENTO-VIDA* = 1
)


; FUNCIONES

(deffunction fun4 ($?x)
    (random 1 4)
)

(deffunction obtener-valor (?obj $?att)
    (bind ?valor (send ?obj (sym-cat get- (nth 1 ?att)))) ;crea el get correspondiente al atributo
    (if (multifieldp ?valor) then ; si es multivaluado
        (nth 1 ?valor)       else
        ?valor
    )
)

(deffunction obtener-valores (?obj $?att)
    (bind ?valor (send ?obj (sym-cat get- (nth 1 ?att))))
    (if (multifieldp ?valor) then
        ?valor               else
        (create$ valor)
    )
)

(deffunction quitar-valor (?obj ?att $?valor)
    (bind ?obj_valor (send ?obj (sym-cat get- ?att)))
    (bind ?nuevo_valor (delete-member$ ?obj_valor (instance-name (nth 1 ?valor)))) ;instance-name porque ?valor es instance-address
    (send ?obj (sym-cat put- ?att) ?nuevo_valor)
)

(deffunction quitar-valores (?obj $?att)
    (bind ?valor (send ?obj (sym-cat get- ?att)))
)

(deffunction modificar-valor (?obj ?att $?valor) 
    (send ?obj (sym-cat put- ?att) ?valor)
)

; El pez se mueve en X o Y tantas unidades como indique su valor en el slot Desplazamiento en una direccion aleatoria
(deffunction calcular-desplazamiento ($?pez)
    (bind ?desplazamiento_pez (obtener-valor (nth 1 ?pez) Desplazamiento))
    (bind ?num (fun4))
    (switch ?num
        (case 1 then (create$ 0 ?desplazamiento_pez))
        (case 2 then (create$ 0 (* -1 ?desplazamiento_pez)))
        (case 3 then (create$ ?desplazamiento_pez 0))
        (case 4 then (create$ (* -1 ?desplazamiento_pez) 0))
    )
)

; El pez se mueve en X e Y tantas unidades como indique su valor en el slot Desplazamiento hacia la comida
; Si el pez se pasaria la comida en X o en Y, se asigna la posicion de la comida (se hace un cap)
(deffunction calcular-desplazamiento-a-comida (?pez $?comida)
    (bind ?comida_instance (nth 1 ?comida))
    (bind ?posicionPezX (obtener-valor ?pez PosX))
    (bind ?posicionPezY (obtener-valor ?pez PosY))
    (bind ?desplazamientoX (obtener-valor ?pez Desplazamiento))
    (bind ?desplazamientoY (obtener-valor ?pez Desplazamiento))
    (bind ?posicionComidaX (obtener-valor ?comida_instance PosX))
    (bind ?posicionComidaY (obtener-valor ?comida_instance PosY))
    (bind ?signoX ">")
    (bind ?signoY ">")
    
    (if (< ?posicionComidaX ?posicionPezX) then
        (bind ?signoX "<")
        (bind ?desplazamientoX (* -1 ?desplazamientoX))
    )

    (if (< ?posicionComidaY ?posicionPezY) then
        (bind ?signoY "<")
        (bind ?desplazamientoY (* -1 ?desplazamientoY))
    )

    (bind ?posicionNuevaPezX (+ ?desplazamientoX ?posicionPezX))
    (bind ?posicionNuevaPezY (+ ?desplazamientoY ?posicionPezY))

    (bind ?condicionX (format nil "(%s %d %d)" ?signoX ?posicionNuevaPezX ?posicionComidaX))
    (bind ?condicionY (format nil "(%s %d %d)" ?signoY ?posicionNuevaPezY ?posicionComidaY))

    (if (eval ?condicionX) then
        (bind ?desplazamientoX (- ?posicionComidaX ?posicionPezX))
    )

    (if (eval ?condicionY) then
        (bind ?desplazamientoY (- ?posicionComidaY ?posicionPezY ))
    )

    (printout t ?desplazamientoX crlf ?desplazamientoY crlf)
    (create$ ?desplazamientoX ?desplazamientoY)
)

(deffunction desplaza-a-posicion (?pez $?desplazamiento)
    (bind ?posicionX (+ (obtener-valor ?pez PosX) (nth 1 ?desplazamiento)))
    (bind ?posicionY (+ (obtener-valor ?pez PosY) (nth 2 ?desplazamiento)))
    (bind ?hambre (obtener-valor ?pez Hambre))
    (modificar-valor ?pez PosX ?posicionX)
    (modificar-valor ?pez PosY ?posicionY)
    (if (< ?hambre 10 ) then
        (modificar-valor ?pez Hambre (+ (obtener-valor ?pez Hambre) 1))
    )
)

; Reduce el hambre del Pez tanto como sea posible según la cantidad
; Reduce la cantidad en lo que se haya consumido
(deffunction comer-pez (?pez $?comida)
    (bind ?comida_instance (nth 1 ?comida))
    (bind ?cantidad_comida (obtener-valor ?comida_instance Cantidad))
    (bind ?hambre_pez (obtener-valor ?pez Hambre))
    (if (< ?cantidad_comida ?hambre_pez) then
        (bind ?consumo ?cantidad_comida) else
        (bind ?consumo ?hambre_pez)
    )
    (modificar-valor ?pez Hambre (- ?hambre_pez ?consumo))
    (modificar-valor ?comida_instance Cantidad (- ?cantidad_comida ?consumo))
    ;incrementar vida
    (bind ?incremento_vida (* ?consumo (obtener-valor ?comida_instance IncrementoVida)))
    (bind ?vida (+ (obtener-valor ?pez Vida) ?incremento_vida))
    (bind ?vida_maxima (obtener-valor ?pez VidaMaxima))

    (if (<= ?vida ?vida_maxima) then
        (modificar-valor ?pez Vida ?vida) else
        (modificar-valor ?pez Vida ?vida_maxima)
    
    )
    
)

(deffunction misma-posicion (?a $?b)
    (bind ?b_instance (nth 1 ?b))
    (bind ?posicionAX (obtener-valor ?a PosX ))
    (bind ?posicionAY (obtener-valor ?a PosY ))
    (bind ?posicionBX (obtener-valor ?b_instance PosX ))
    (bind ?posicionBY (obtener-valor ?b_instance PosY ))
    (and (eq ?posicionAX ?posicionBX ) (eq ?posicionAY ?posicionBY))
)

(deffunction comprobarLimites (?pez ?pecera $?desplazamiento)
    (bind ?posicionPezX (+ (obtener-valor ?pez PosX) (nth 1 ?desplazamiento)))
    (bind ?posicionPezY (+ (obtener-valor ?pez PosY) (nth 2 ?desplazamiento)))
    (bind ?ancho (obtener-valor ?pecera Ancho ))
    (bind ?alto (obtener-valor ?pecera Alto ))

    (and (and (<= ?posicionPezX ?ancho) (>= ?posicionPezX 0)) (and (<= ?posicionPezY ?alto) (>= ?posicionPezY 0))) 
)

(deffunction incrementar-tiempo ($?mundo)
    (bind ?actual (obtener-valor (nth 1 ?mundo) Reloj))
    (modificar-valor (nth 1 ?mundo) Reloj (+ ?actual 1))
)


(deffunction decrementar-vida ($?pez)
    (bind ?actual (obtener-valor (nth 1 ?pez) Vida))
    (modificar-valor (nth 1 ?pez) Vida (- ?actual ?*DECREMENTO-VIDA*))
)

; REGLAS
;(Reloj ?t &:(<> 0 (mod ?t 2)))

(defrule TIEMPO
    ?mundo <- (object (is-a Mundo_peces)) 
    =>
    (incrementar-tiempo ?mundo)
)

(defrule MORIR
    ?pez <- (object (is-a Pez) (Vida ?v &:(= ?v 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    => (quitar-valor ?mundo Peces ?pez)
    (incrementar-tiempo ?mundo)
)

(defrule COMER
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6) ) (Vida ?v &:(> ?v 0))) ;&: (<> ?h 10)
    ?comida <- (object (is-a Comida) (Cantidad ?c &: (> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (misma-posicion ?comida ?pez))
    =>
    (comer-pez ?pez ?comida)
    (incrementar-tiempo ?mundo)
)



(defrule MOVER-A-COMIDA
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6) &: (<> ?h 10)  ) (Vida ?v &:(> ?v 0)) ) ;
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (not (misma-posicion ?comida ?pez)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento-a-comida ?pez ?comida))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (incrementar-tiempo ?mundo)
)

(defrule MOVER
    ?pez <- (object (is-a Pez)(Hambre ?h &: (<= ?h 6)) (Vida ?v &:(> ?v 0)));
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento ?pez))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (incrementar-tiempo ?mundo)
)

(defrule MOVERSE-A-COMIDA-HAMBRIENTO
    ?pez <- (object (is-a Pez) (Hambre ?h &:(= ?h 10)) (Vida ?v &:(> ?v 0)) ) ;
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (not (misma-posicion ?comida ?pez)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento-a-comida ?pez ?comida))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (decrementar-vida ?pez)
    (incrementar-tiempo ?mundo)


)


(defrule MOVERSE-HAMBRIENTO
    ?pez <- (object (is-a Pez) (Hambre ?h &:(> ?h 6))  (Vida ?v &:(> ?v 0)) )
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(<= ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento ?pez))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (if (= (obtener-valor ?pez Hambre ) 10) then
        (decrementar-vida ?pez)
    )
    (decrementar-vida ?pez)
    (incrementar-tiempo ?mundo)

)

(defrule AMAZON-AL-RESCATE
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(<= ?c 0) &:(>  ?c ?*TIEMPO-RECARGA*)))
    ?mundo <- (object (is-a Mundo_peces)  (Reloj ?t &:(> ?t 0)))
    => 
    (modificar-valor ?comida Cantidad (- (obtener-valor ?comida Cantidad) 1))
    (incrementar-tiempo ?mundo)
)

(defrule SEUR-HA-LLEGADO-Y-ESTABAS-EN-CASA
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(=  ?c ?*TIEMPO-RECARGA*)))
    ?mundo <- (object (is-a Mundo_peces)  (Reloj ?t &:(> ?t 0)))
    => 
   (modificar-valor ?comida Cantidad 20)
   (incrementar-tiempo ?mundo)
) 


;[X] que al mover les entre hambre
;[ ] sistema de sexos
;[ ] reproducción por cercanía y nivel de hambre -> rule ovular
;[ ] Agresivo si sexo igual mientras reproduce
;[X] Vida de los peces
;[X] La comida restaura una cierta cantidad de vida 
;[X] Una vez tengas 10 de hambre la vida se va reduciendo x unidades
