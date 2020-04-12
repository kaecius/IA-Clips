;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pont")
;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pins")

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
    (modificar-valor ?pez PosX ?posicionX)
    (modificar-valor ?pez PosY ?posicionY)
    (modificar-valor ?pez Hambre (+ (obtener-valor ?pez Hambre) 1))
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

; REGLAS
;(Reloj ?t &:(<> 0 (mod ?t 2)))

(defrule TIEMPO
    ?mundo <- (object (is-a Mundo_peces)) 
    =>
    (incrementar-tiempo ?mundo)
)

(defrule MORIR
    ?pez <- (object (is-a Pez) (Hambre 10))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    => (quitar-valor ?mundo Peces ?pez)
    (incrementar-tiempo ?mundo)
)

(defrule MOVER
    ?pez <- (object (is-a Pez) (Hambre ?h &: (<= ?h 6)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento ?pez))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (incrementar-tiempo ?mundo)
)

(defrule MOVER-A-COMIDA
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6) &: (<> ?h 10)))
    ?comida <- (object (is-a Comida))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (not (misma-posicion ?comida ?pez)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento-a-comida ?pez ?comida))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (incrementar-tiempo ?mundo)
)

(defrule COMER
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6) &: (<> ?h 10)))
    ?comida <- (object (is-a Comida) (Cantidad ?c &: (> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (misma-posicion ?comida ?pez))
    => 
    ;(modificar-valor ?pez PosX (obtener-valor ?comida PosX))
    ;(modificar-valor ?pez PosY (obtener-valor ?comida PosY))    
    (comer-pez ?pez ?comida)
    (incrementar-tiempo ?mundo)
)

;[X] que al mover les entre hambre
;[ ] sistema de sexos
;[ ] reproducción por cercanía y nivel de hambre -> rule ovular
;[ ] Agresivo si sexo igual mientras reproduce
;[ ] Vida de los peces
;[ ] La comida restaura una cierta cantidad de vida 
;[ ] Una vez tengas 10 de hambre la vida se va reduciendo x unidades
