;;;EXPLICACIÓN DE UN ESTADO ARBITRARIO
; Un estado arbitrario es una lista (x y d cop op1... opk) donde 
;x es el contenido del primer recipiente, 
;y es el contenido del segundo 
;cop es la abraviatura de "cadena de operadores" e indica que a continuación 
;se guarda la cadena de operadores que lleva desde el estado inicial al actual

(deffunction extrae-cop ($?estado)
	(bind ?posicion-cop (member$ cop ?estado))
	(bind ?ultima (length$ ?estado))
	(subseq$ ?estado  ?posicion-cop ?ultima)
)

(deffunction estado-actual ($?estado)
	(bind ?posicion-cop (member$ cop ?estado))
	(subseq$ ?estado 1 (- ?posicion-cop 1))
)

(defglobal 
	?*TX* = 4
	?*TY* = 3
	?*ESTADO-INICIAL* = (create$ 0 0 cop)
	?*OPERADORES* = (create$ llenar-x llenar-y volcar-x-y volcar-y-x tirar-x tirar-y)
	?*LISTA* = (create$   (implode$ ?*ESTADO-INICIAL*))
	?*PADRE* = ?*ESTADO-INICIAL*
	?*PASOS* = 0 ;indica que no hay límite en el número de pasos
	?*CON-PROHIBIDO* = TRUE
	?*CON-VISITADOS* = TRUE
	?*VISITADOS* = (create$ (implode$ (estado-actual ?*ESTADO-INICIAL*)))
)

(deffunction cantidad-x ($?estado)
	(nth$ 1 ?estado)
)

(deffunction cantidad-y ($?estado)
	(nth$ 2 ?estado)
)

;PROHIBIDO: Llenar x si ya esta lleno
(deffunction llenar-x ($?estado)
(if (eq ?estado PROHIBIDO) 
	then PROHIBIDO
	else (if (and (= (nth$ 1 ?estado) ?*TX*) ?*CON-PROHIBIDO*) 
		then PROHIBIDO
		else 	
			(bind ?estado(replace$ ?estado 1 1  ?*TX*))
			(create$ ?estado llenar-x)
		)
	)
)

;PROHIBIDO: LLenar y si ya esta lleno
(deffunction llenar-y ($?estado)
(if (eq ?estado PROHIBIDO) 
	then PROHIBIDO
	else (if (and (= (nth$ 2 ?estado) ?*TY*) ?*CON-PROHIBIDO*)
			then PROHIBIDO
			else 	
			(bind ?estado(replace$ ?estado 2 2  ?*TY*))
			(create$ ?estado llenar-y)
	     )
	)
)

;Comprueba si no se puede volcar el primer recipiente en el segundo
(deffunction prohibido-volcar? (?x ?y $?cmp)
	(if (and (or (= ?x 0) (= (nth 1 ?cmp) ?y)) ?*CON-PROHIBIDO*)
		then TRUE
		else FALSE
	)
)

(deffunction volcar-x-y ($?estado)
	(if (eq ?estado PROHIBIDO) 
		then PROHIBIDO
		else 	
		(bind ?cop (extrae-cop ?estado))
		(bind ?x (cantidad-x ?estado))
		(bind ?y (cantidad-y ?estado))
		(bind ?falta-y (- ?*TY* ?y)) ;lo que falta para llenar y
		;Si lo que hay en x es <= que lo que falta para llenar y
			;entonces (x'=0, y'=x+y)
		(if (not (prohibido-volcar? ?x ?y ?*TY*))
			then (if (<= ?x ?falta-y) 
					then (bind ?nvo (create$ 0 (+ ?x ?y)))
					else
					;lo que hay en x es más que lo que falta para llenar y
					;entonces (x'=x-falta-y, y'=*TY*)
					(bind ?nvo(create$ (- ?x ?falta-y) ?*TY*))
				)
				(create$ ?nvo ?cop volcar-x-y)
			else PROHIBIDO
		)
	)
)

(deffunction volcar-y-x ($?estado)
	(if (eq ?estado PROHIBIDO) 
		then PROHIBIDO
		else 	
		(bind ?cop (extrae-cop ?estado))
		(bind ?x (cantidad-x ?estado))
		(bind ?y (cantidad-y ?estado))
		(bind ?falta-x (- ?*TX* ?x)) ;lo que falta para llenar x
		;Si lo que hay en y es <= que lo que falta para llenar x
			;entonces (x'=x+y, y'=0)
		(if (not (prohibido-volcar? ?y ?x ?*TX*))
			then (if (<= ?y ?falta-x) 
					then (bind ?nvo(create$ (+ ?x ?y) 0))
					else
					;lo que hay en y es m�s que lo que falta para llenar x
					;entonces (x'=*TX* , y'=y - falta-x)
					(bind ?nvo(create$  ?*TX* (- ?y ?falta-x)))
				)
				(create$ ?nvo ?cop volcar-y-x)
			else PROHIBIDO
		)
	)
)

(deffunction tirar-x(?estado)
	(if (eq ?estado PROHIBIDO) 
		then PROHIBIDO
		else
		(if (and (= (nth 1 ?estado) 0) ?*CON-PROHIBIDO*)
			then PROHIBIDO
			else 
				(bind ?nvo (create$ 0 (cantidad-y ?estado)))
				(create$ ?nvo (extrae-cop ?estado) tirar-x)	
		)
		
	)
)

(deffunction tirar-y(?estado)
	(if (eq ?estado PROHIBIDO) 
		then PROHIBIDO
		else
			(if (and (= (nth 2 ?estado) 0) ?*CON-PROHIBIDO*)
				then PROHIBIDO
				else 
					(bind ?nvo (create$  (cantidad-x ?estado) 0))
					(create$ ?nvo (extrae-cop ?estado) tirar-y)
			)
	)
)			

(deffunction prohibido? ($?estado)
  	(eq $?estado (create$ PROHIBIDO))
)

(deffunction exito (?estado)
	(or 
		(and(= (cantidad-x ?estado)2)(= (cantidad-y ?estado)0))
		(and(= (cantidad-x ?estado)0)(= (cantidad-y ?estado)2))
	)
)

;;;;***************** funciones de búsqueda

(deffunction aplicar-operador (?operador $?estado)
	(funcall  ?operador $?estado)
)

(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind ?hijo (aplicar-operador ?op ?estado))
		; Se añaden los hijos siempre que:
		; 1 -> No sean prohibidos
		; 2 -> Bien no haya visitados
		;      Bien haya visitados, pero el hijo no haya sido visitado
		(if (and (not (prohibido? ?hijo)) (or (not ?*CON-VISITADOS*) (and ?*CON-VISITADOS* (not (member$ (implode$ (estado-actual ?hijo)) ?*VISITADOS*))))) then 
			(bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo)))
			(if ?*CON-VISITADOS* then (bind ?*VISITADOS* (create$ ?*VISITADOS* (implode$ (estado-actual ?hijo)))))
		)
	)
	?lista-hijos
)

(deffunction busqueda-en-profundidad ($?lista)
	(bind ?i 0)
	(while (and (not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
		(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1  ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then
			(bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?hijos ?*LISTA*)))
		(bind ?i (+ ?i 1))
		(if (and (> ?*PASOS* 0) (= ?i ?*PASOS*)) then (break))
	)

	(if  (exito ?*PADRE*) 
		then (printout t "La solución es " ?*PADRE* crlf)
		else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
	)
)

(deffunction busqueda-en-anchura ($?lista)
	(bind ?i 0)
	(while (and (not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
		(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1 ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then
			(bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?*LISTA* ?hijos)))
		(bind ?i (+ ?i 1))
		(if (and (> ?*PASOS* 0) (= ?i ?*PASOS*)) then (break))
	)
	(if  (exito ?*PADRE*) 
		then (printout t "La solución es " ?*PADRE* crlf)
		else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
	)
)
