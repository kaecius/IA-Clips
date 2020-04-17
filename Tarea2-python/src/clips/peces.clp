;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pont")
;(load "Z:/home/xabilahu/Documentos/EHU/Tercero/SegundoCuatrimestre/IA/IA-Clips/Tarea2-python/src/clips/peces.pins")

; (set-dynamic-constraint-checking) Validación dinámica de facets restrictivos (p.e. range, allowed-classes, allowed-values)

(defglobal
    ?*TIEMPO-RECARGA* = (* -1 5)
    ?*DECREMENTO-VIDA* = 1
    ?*CANTIDAD-TRANSPORTE* = 20
    ?*RANGO-REPRODUCCION* = 100
    ?*NUMERO-HIJOS* = 0
    ?*MULTIPLICADOR* = 2
    ?*INMUNIDAD* = 3
    ?*COSTE-REPRODUCCION* = 3
    ?*UMBRAL-VIDA* = 5
    ?*PRIORIDAD* = 10000
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
    (bind ?*PRIORIDAD* (- ?*PRIORIDAD* 1))
)

(deffunction prioridad-tiempo ($?x)
    (- ?*PRIORIDAD* 1)
)

(deffunction decrementar-vida ($?pez)
    (bind ?actual (obtener-valor (nth 1 ?pez) Vida))
    (modificar-valor (nth 1 ?pez) Vida (- ?actual ?*DECREMENTO-VIDA*))
)

(deffunction distancia-mahattan (?a $?b)
    (bind ?b_instance (nth 1 ?b))
    (bind ?x1 (obtener-valor ?a PosX))
    (bind ?x2 (obtener-valor ?b_instance PosX))
    (bind ?y1 (obtener-valor ?a PosY))
    (bind ?y2 (obtener-valor ?b_instance PosY))
    (+ (abs (- ?x1 ?x2)) (abs (- ?y1 ?y2)))
)

; Se crea una lista con los peces Macho que pueden reproducirse
(deffunction machos-en-rango (?h $?pecera)
    (bind ?candidatos (create$))
    (progn$ (?pez ?pecera)
        (if (and (eq (obtener-valor ?pez Sexo) Macho) 
                 (= (obtener-valor ?pez RecienNacido) 0)
                 (< (obtener-valor ?pez Hambre) 10)
                 (<= (distancia-mahattan ?h ?pez) ?*RANGO-REPRODUCCION*)) then
                     (bind ?candidatos (create$ ?candidatos ?pez))
        )
    )
    ?candidatos
)

; La mecánica de peleas es la siguiente:
;   - Mientras que haya más de 1 pez, se eligen dos peces de manera aleatoria.
;   - Uno de los peces elegidos ataca al otro.
;   - Los peces dejan de pelear cuando su vida <= ?*UMBRAL-VIDA*.
(deffunction pelear ($?c)
    (bind ?listaCandidatos (create$ ?c))
    (while (<> 1 (length$ ?listaCandidatos)) do
        (bind ?cantidadPeces (length$ ?listaCandidatos))
        (bind ?atacante (random 1 ?cantidadPeces))
        (bind ?atacado (random 1 ?cantidadPeces))
        (if (<> ?atacante ?atacado) then 
            (modificar-valor (nth ?atacado ?listaCandidatos) Vida (* -1 (obtener-valor (nth ?atacante ?listaCandidatos) Fuerza)))
            (if (<= (obtener-valor (nth ?atacado ?listaCandidatos) Vida) ?*UMBRAL-VIDA*) then
                (bind ?listaCandidatos (delete$ ?listaCandidatos ?atacado ?atacado))
            )
        )
    )
    (nth 1 ?listaCandidatos)
)

; Esta función calcula un multiplicador aleatorio perteneciente al conjunto {0.1, 0.2, 0.3, ... , ?*MULTIPLICADOR*}.
(deffunction multiplicador-caracteristica-hijo ($?x)
    (bind ?entero (random 0 ?*MULTIPLICADOR*))
    (bind ?decimal (random 0 9))
    (bind ?mult (float (string-to-field (format nil "%d.%d" ?entero ?decimal))))
    (if (= ?mult 0) then (bind ?mult 1.0))
    ?mult
)

; Se crea un hijo de la siguiente manera:
;   - Se calcula un multiplicador aleatorio perteneciente al conjunto {0.1, 0.2, 0.3, ... , ?*MULTIPLICADOR*}.
;   - La VidaMaxima, Fuerza y Desplazamiento de calculan como promedio de sus progenitores, multiplicado por el multiplicador.
;   - El sexo del hijo se decide aleatoriamente, la pobabilidad de cada sexo es 50%.
;   - La posición inicial del hijo es el punto intermedio entre los progenitores.
;   - Los hijos no podrán pelearse ni reproducirse hasta que pasen ?*INMUNIDAD* ticks de reloj.
;   - Los progenitores suman ?*COSTE-REPRODUCCION* unidades de Hambre.
(deffunction crear-hijo (?h $?m)
    (bind ?m_instance (nth 1 ?m))
    (bind ?mult (multiplicador-caracteristica-hijo))
    (bind ?vida (integer (* (div (+ (obtener-valor ?h VidaMaxima) (obtener-valor ?m_instance VidaMaxima)) 2) ?mult)))

    (if (> ?vida 100) then (bind ?vida 100))
    (if (< ?vida 1) then (bind ?vida 1))

    (bind ?fuerza (integer (* (div (+ (obtener-valor ?h Fuerza) (obtener-valor ?m_instance Fuerza)) 2) ?mult)))

    (if (> ?fuerza 100) then (bind ?fuerza 100))
    (if (< ?fuerza 1) then (bind ?fuerza 1))

    (bind ?desplazamiento (integer (* (div (+ (obtener-valor ?h Desplazamiento) (obtener-valor ?m_instance Desplazamiento)) 2) ?mult)))
    
    (if (< ?desplazamiento 1) then (bind ?desplazamiento 50))

    (if (= (random 0 1) 1) then
        (bind ?sexo Hembra) else
        (bind ?sexo Macho)
    )

    (bind ?posx (div (+ (obtener-valor ?h PosX) (obtener-valor ?m_instance PosX)) 2))
    (bind ?posy (div (+ (obtener-valor ?h PosY) (obtener-valor ?m_instance PosY)) 2))

    ; Sumar Hambre
    (bind ?progenitores (create$ ?h ?m))
    (progn$ (?p ?progenitores)
        (bind ?hambre (obtener-valor ?p Hambre))
        (bind ?aumento ?*COSTE-REPRODUCCION*)
        (if (> (+ ?hambre ?aumento) 10) then
            (bind ?aumento (- 10 ?hambre))
        )
        (modificar-valor ?p Hambre (+ ?hambre ?aumento))
    )

    (bind ?*NUMERO-HIJOS* (+ ?*NUMERO-HIJOS* 1))

    (make-instance (sym-cat hijo- ?*NUMERO-HIJOS*) of Pez 
        (VidaMaxima ?vida) (Sexo ?sexo)
        (Fuerza ?fuerza) (Desplazamiento ?desplazamiento)
        (PosX ?posx) (PosY ?posy) (RecienNacido ?*INMUNIDAD*)
    )
)

; DAEMONS

; Daemon que inicializa la Vida del pez a su VidaMaxima
(defmessage-handler Pez init after ()
    (bind ?self:Vida ?self:VidaMaxima)
)

; Daemon que actualiza los peces recién nacidos cada tick de reloj
(defmessage-handler Mundo_peces put-Reloj after (?incremento)
    (progn$ (?pez ?self:Peces)
        (bind ?rnacido (obtener-valor ?pez RecienNacido))
        (if (> ?rnacido 0) then
            (modificar-valor ?pez RecienNacido (- ?rnacido 1))
        )
    )
)

; REGLAS

(defrule TIEMPO
    (declare (salience (prioridad-tiempo)))
    ?mundo <- (object (is-a Mundo_peces) (Reloj ?x &: (>= ?x 0))) 
    =>
    (incrementar-tiempo ?mundo)
)

(defrule MORIR
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez) (Vida ?v &:(<= ?v 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    => (quitar-valor ?mundo Peces ?pez)
    ; (incrementar-tiempo ?mundo)
)

(defrule ESPERAR-RELLENO-COMIDA
    (declare (salience ?*PRIORIDAD*))
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(<= ?c 0) &:(>  ?c ?*TIEMPO-RECARGA*)))
    ?mundo <- (object (is-a Mundo_peces) (Reloj ?t &:(> ?t 0)))
    => 
    (modificar-valor ?comida Cantidad (- (obtener-valor ?comida Cantidad) 1))
    ; (incrementar-tiempo ?mundo)
)

(defrule RELLENAR-COMIDA
    (declare (salience ?*PRIORIDAD*))
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(=  ?c ?*TIEMPO-RECARGA*)))
    ?mundo <- (object (is-a Mundo_peces) (Reloj ?t &:(> ?t 0)))
    => 
   (modificar-valor ?comida Cantidad ?*CANTIDAD-TRANSPORTE*)
;    (incrementar-tiempo ?mundo)
)

(defrule COMER
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6)) (Vida ?v &:(> ?v 0)))
    ?comida <- (object (is-a Comida) (Cantidad ?c &: (> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (misma-posicion ?comida ?pez))
    =>
    (comer-pez ?pez ?comida)
    ; (incrementar-tiempo ?mundo)
)

(defrule CANDIDATOS-REPRODUCIRSEç
    (declare (salience ?*PRIORIDAD*))
    ?hembra <- (object (is-a Pez) (Sexo Hembra) (Hambre ?h &: (< ?h 10)) (RecienNacido ?r &: (= ?r 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?hembra (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?hembra_instance (instance-name ?hembra))
    (bind ?candidatos (machos-en-rango ?hembra_instance (create$ ?x ?z)))
    (if (> (length$ ?candidatos) 1) then
        (assert (hembra ?hembra_instance candidatos ?candidatos)) else
        (if (= (length$ ?candidatos) 1) then
            (assert (reproducir ?hembra_instance (nth 1 ?candidatos)))
        )
    )
    ; (incrementar-tiempo ?mundo)
)

(defrule PELEA
    (declare (salience ?*PRIORIDAD*))
    ?hecho <- (hembra ?h candidatos ?c)
    ?mundo <- (object (is-a Mundo_peces) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?ganador (pelear ?c))
    (retract ?hecho)
    (assert (reproducir ?h ?ganador))
    ; (incrementar-tiempo ?mundo)
)

(defrule REPRODUCIRSE
    (declare (salience ?*PRIORIDAD*))
    ?hecho <- (reproducir ?h ?m)
    ?mundo <- (object (is-a Mundo_peces) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?hijo (crear-hijo ?h ?m))
    (bind ?peces (obtener-valores ?mundo Peces))
    (modificar-valor ?mundo Peces (create$ ?peces ?hijo))
    (retract ?hecho)
    ; (incrementar-tiempo ?mundo)
)

(defrule MOVER-A-COMIDA
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez) (Hambre ?h &: (> ?h 6) &: (<> ?h 10)) (Vida ?v &:(> ?v 0)))
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (not (misma-posicion ?comida ?pez)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento-a-comida ?pez ?comida))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    ; (incrementar-tiempo ?mundo)
)

(defrule MOVER
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez)(Hambre ?h &: (<= ?h 6)) (Vida ?v &:(> ?v 0)));
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento ?pez))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    ; (incrementar-tiempo ?mundo)
)

(defrule MOVERSE-A-COMIDA-HAMBRIENTO
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez) (Hambre ?h &:(= ?h 10)) (Vida ?v &:(> ?v 0)) ) ;
    ?comida <- (object (is-a Comida) (Cantidad ?c &:(> ?c 0)))
    ?mundo <- (object (is-a Mundo_peces) (Peces $?x ?y $?z &: (eq ?pez (instance-address ?y))) (Reloj ?t &:(> ?t 0)))
    (test (not (misma-posicion ?comida ?pez)))
    =>
    (bind ?desplazamiento (calcular-desplazamiento-a-comida ?pez ?comida))
    (if (comprobarLimites ?pez (obtener-valor ?mundo Mundo) ?desplazamiento) then 
        (desplaza-a-posicion ?pez ?desplazamiento))
    (decrementar-vida ?pez)
    ; (incrementar-tiempo ?mundo)
)

(defrule MOVERSE-HAMBRIENTO
    (declare (salience ?*PRIORIDAD*))
    ?pez <- (object (is-a Pez) (Hambre ?h &:(> ?h 6)) (Vida ?v &:(> ?v 0)) )
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
    ; (incrementar-tiempo ?mundo)
)

;[X] que al mover les entre hambre
;[X] sistema de sexos
;[X] reproducción por cercanía y nivel de hambre
;[X] Agresivo si sexo igual mientras reproduce
;[X] Vida de los peces
;[X] La comida restaura una cierta cantidad de vida 
;[X] Una vez tengas 10 de hambre la vida se va reduciendo x unidades
