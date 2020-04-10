(defglobal 
    ?*DESPLAZAMIENTO* = 80
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

(deffunction calcular-desplazamiento ($?x)
    (bind ?num (fun4))
    (switch ?num
        (case 1 then (create$ 0 ?*DESPLAZAMIENTO*))
        (case 2 then (create$ 0 (* -1 ?*DESPLAZAMIENTO*)))
        (case 3 then (create$ ?*DESPLAZAMIENTO* 0))
        (case 4 then (create$ (* -1 ?*DESPLAZAMIENTO*) 0))
    )
)

(deffunction desplaza-a-posicion (?pez $?desplazamiento)
    (bind ?alto (obtener-valor [pecera_standard] Alto ))
    (bind ?posicionX (+ (obtener-valor ?pez PosX) (nth 1 ?desplazamiento)))
    (bind ?posicionY (+ (obtener-valor ?pez PosY) (nth 2 ?desplazamiento)))
    (modificar-valor ?pez PosX ?posicionX)
    (modificar-valor ?pez PosY ?posicionY)
)

(deffunction comer-pez (?pez $?comida)
    (bind ?comida_instance (nth 1 ?comida))
    (bind ?posicionPezX (obtener-valor ?pez PosX ))
    (bind ?posicionPezY (obtener-valor ?pez PosY ))
    (bind ?posicionComidaX (obtener-valor ?comida_instance PosX ))
    (bind ?posicionComidaY (obtener-valor ?comida_instance PosY ))

    (if (and (eq ?posicionPezX ?posicionComidaX ) (eq ?posicionPezY ?posicionComidaY)) then
        (bind ?cantidad_comida (obtener-valor ?comida_instance Cantidad))
        (bind ?hambre_pez (obtener-valor ?pez Hambre))
        (if (< ?cantidad_comida ?hambre_pez) then
            (bind ?consumo ?cantidad_comida) else
            (bind ?consumo ?hambre_pez)
        )
        (modificar-valor ?pez Hambre (- ?hambre_pez ?consumo))
        (modificar-valor ?comida_instance Cantidad (- ?cantidad_comida ?consumo))
    )
)

(deffunction comprobarLimites (?pez ?pecera $?desplazamiento)
    (bind ?posicionPezX (+ (obtener-valor ?pez PosX) (nth 1 ?desplazamiento)))
    (bind ?posicionPezY (+ (obtener-valor ?pez PosY) (nth 2 ?desplazamiento)))
    (bind ?ancho (obtener-valor ?pecera Ancho ))
    (bind ?alto (obtener-valor ?pecera Alto ))

    (and (and (<= ?posicionPezX ?ancho) (>= ?posicionPezX 0)) (and (<= ?posicionPezY ?alto) (>= ?posicionPezY 0))) 
)

; REGLAS

(defrule MORIR
    ?pez <- (object (is-a Pez) (Hambre 10))
    ?mundo <- (object (is-a Mundo_peces))
    => (quitar-valor ?mundo Peces ?pez)
)

(defrule MOVER
    ?pez <- (object (is-a Pez) (Hambre ?x &: (<= ?x 6)))
    ?pecera <- (object (is-a Pecera))
    => 
    (bind ?desplazamiento (calcular-desplazamiento))
    (if (comprobarLimites ?pez ?pecera ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
)

(defrule COMER
    ?pez <- (object (is-a Pez) (Hambre ?x &: (> ?x 6) &: (<> ?x 10)))
    ?comida <- (object (is-a Comida))
    => 
    (modificar-valor ?pez PosX (obtener-valor ?comida PosX))
    (modificar-valor ?pez PosY (obtener-valor ?comida PosY))    
    (comer-pez ?pez ?comida)
)
