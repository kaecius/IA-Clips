;;;EXPLICACI�N DE UN ESTADO ARBITRARIO
; Un estado arbitrario es una lista (x y d cop op1... opk) donde 
;x es el contenido del primer recipiente, 
;y es el contenido del segundo 
;d es la cantidad de agua vertida por el desag�e
;cop es la abraviatura de "cadena de operadores" e indica que a continuaci�n 
;se guarda la cadena de operadores que lleva desde el estado inicial al actual
(defglobal 
 ?*TX* = 4
 ?*TY* = 3
 ?*ESTADO-INICIAL* = (create$ 0 0 cop)
 ?*OPERADORES* = (create$ llenar-x llenar-y volcar-x-y volcar-y-x tirar-x tirar-y)
 ?*LISTA* = (create$   (implode$ ?*ESTADO-INICIAL*))
 ?*PADRE* = ?*ESTADO-INICIAL*
 ?*PASOS* = 0 ;indica que no hay l�mite en el n�mero de pasos
 ?*CON-PROHIBIDO* = TRUE
)

(deffunction cantidad-x ($?estado)
(nth$ 1 ?estado)
)

(deffunction cantidad-y ($?estado)
(nth$ 2 ?estado)
)

(deffunction extrae-cop ($?estado)
(bind ?posicion-cop (member$ cop ?estado))
(bind ?ultima (length$ ?estado))
(subseq$ ?estado  ?posicion-cop ?ultima)
)

(deffunction estado-actual ($?estado)
(bind ?posicion-cop (member$ cop ?estado))
(subseq$ ?estado 1 (- ?posicion-cop 1))
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
	))


;;;heuristicos

;
;;;heur�stico dado en el examen

(deffunction h-examen(?estado)
  (abs(-(+ (cantidad-x ?estado) (cantidad-y ?estado)) 2)))


;;;costes

(defglobal
	?*FUNCION-g* = ""
	?*FUNCION-h* = ""
	?*VALORES-f* = (create$)
  	?*VALORES-g* = (create$)
  	?*VALORES-h* = (create$)
  	?*MIN* = nil
  	?*POSMIN* = 1
  	?*ULTIMO-ESTADO* = (implode$ (estado-actual ?*PADRE*))
  	?*VISITADOS* = (create$)	
)

;;Funci�n de coste uniforme 1 (longitud del camino)

(deffunction coste-1(?estado)
(length$ (rest$(extrae-cop ?estado)))
)

;;;funcion constante cero

(deffunction cero (?estado) 0)

;;;evaluaci�n de costes: definir funciones g, h y f=g+h, as� como las listas de valores 
;
(deffunction g ($?estado) (eval (format nil ?*FUNCION-g* (nth 1 ?estado))))
(deffunction h ($?estado) (eval (format nil ?*FUNCION-h* (nth 1 ?estado))))
(deffunction f ($?estado) (+(g (nth 1 ?estado)) (h (nth 1 ?estado))))

;;;;;;;;;;;B�squeda guiada por informaci�n, funciones auxiliares

;;;la lista  ?*VALORES-f* contiene los valores f=g+h de cada estado de ?*LISTA

;;C�lculo de los valores de las funciones f, g, h (se pasan como par�metro) para toda la ?*LISTA* estados (como strings)
(deffunction valores-funcion-lista (?funcion $?lista)
(bind ?resultado-funcion (create$))
(progn$ (?elemento ?lista) 
	; (bind ?estado (explode$ ?elemento))
	(bind ?resultado-funcion (create$ ?resultado-funcion (funcall ?funcion ?elemento)))

)
?resultado-funcion)

;;;;***************** funciones de b�squeda
;
(deffunction aplicar-operador (?operador $?estado)
(funcall ?operador $?estado)
)

;;;La misma funci�n aplicar operador implementada de otra forma
;;(deffunction aplicar-operador (?operador $?estado)
;;(eval
;;(format nil "( %s (create$ %s))" ?operador (implode$ ?estado))
;;)
;;)

(deffunction operadores-hijos($?estado)
(bind $?lista-operadores (create$))
(progn$ (?op ?*OPERADORES*) 
	(bind $?hijo (aplicar-operador ?op ?estado))
	(if (not (prohibido? ?hijo)) then 
		(bind ?lista-operadores (create$ ?lista-operadores ?op))))
?lista-operadores)



(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (not (prohibido? ?hijo)) then 
			(bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo))))
	)
?lista-hijos)


;;esta funci�n busca el m�nimo de una lista incluso si tiene menos de 2 elementos
(deffunction minimum ($?v)
(if (=(length$ ?v) 1) then (nth$ 1 ?v) else 
(eval(format nil "(min %s)" (implode$ ?v)))
))	

;;;;;;;;;;;***********BUSQUEDA INFORMADA*****************

(deffunction busqueda_informada_con_visitados (?g ?h $?save_exec)
(bind ?i 1)
(bind ?*CON-PROHIBIDO* TRUE)
(bind ?*FUNCION-g* (format nil "(funcall %s " ?g))
(bind ?*FUNCION-g* (str-cat ?*FUNCION-g* "(create$ %s))"))
(bind ?*FUNCION-h* (format nil "(funcall %s " ?h))
(bind ?*FUNCION-h* (str-cat ?*FUNCION-h* "(create$ %s))"))
(bind ?*VALORES-g* (create$ (g (implode$ ?*ESTADO-INICIAL*))))
(bind ?*VALORES-h* (create$ (h (implode$ ?*ESTADO-INICIAL*))))
(bind ?*VALORES-f* (create$ (f (implode$ ?*ESTADO-INICIAL*))))
(bind ?*MIN* (minimum ?*VALORES-f*))

(bind ?save_exec (nth 1 ?save_exec))

(if (not (eq ?save_exec t))
	then (printout t "Introduzca el nombre del fichero donde se almacenará la ejecución: ")
		 (bind ?filename (read))
		 (open ?filename ?save_exec "w")
)

(while (and (not (= ?*PASOS* ?i))   (not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
	
;contador de n�mero de pasos
(printout ?save_exec crlf "-------Paso " ?i "-------" crlf)

   (while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) 
;borrar el padre de la posici�n ?*POSMIN* y los valores de esa posici�n en todas las listas
	(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-f*(delete$ ?*VALORES-f* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
	
	(if (eq ?*LISTA* (create$)) then (break))
	;buscar el nuevo padre
	(bind ?*MIN* (minimum ?*VALORES-f*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-f*))
	(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
;Ahora obtenemos el estado visitado simplemente quitando el camino de operadores
	(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))
;con el bucle as� organizado el contador ?i no avanza

	)

(if (not(eq ?*LISTA* (create$))) then 

 ;Ahora tenemos la seguridad de que al salir del bucle, el �ltimo-estado no ha sido visitado
;lo metemos en visitados
	(bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO* ))

;Imprimimos los mensajes para saber valores de f=g+h
	
	(printout ?save_exec "Padre = " ?*PADRE* crlf crlf)
	(printout ?save_exec tab "g = " (nth ?*POSMIN* ?*VALORES-g*)
				tab "h = " (nth ?*POSMIN* ?*VALORES-h*)
				tab "f = g + h = " ?*MIN* crlf)
				
;Borramos ahora el padre no visitado y los valores f=g+h
;Incluimos sus hijos y los valores f=g+h en las correspondientes listas, en profundidad
	(if (not (exito ?*PADRE*)) then
	        (bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?hijos ?*LISTA*))
			(bind ?*POSMIN* (+ ?*POSMIN* (length$ ?hijos))) ; por ser profundidad, al añadir en modo pila se debe sumar a posmin la cantidad de los elementos añadidos
			(bind ?*VALORES-g* (create$ (valores-funcion-lista g ?hijos) ?*VALORES-g*))
			(bind ?*VALORES-h* (create$ (valores-funcion-lista h ?hijos) ?*VALORES-h*))
			(bind ?*VALORES-f* (create$ (valores-funcion-lista f ?hijos) ?*VALORES-f*))
;incrementamos el contador
(printout ?save_exec crlf tab "?*VISITADOS* = " ?*VISITADOS* crlf)
(printout ?save_exec crlf tab "?*LISTA* = " ?*LISTA*  crlf crlf)
	(bind ?i (+ ?i 1))
)
)
)
;;Salimos del bucle porque el padre sea �xito o porque no haya soluci�n
(if  (exito ?*PADRE*) then (printout ?save_exec crlf "La solución es " ?*PADRE* crlf)
else (if (=(length$ ?*LISTA*)0)  then (printout ?save_exec crlf "No hay solución." crlf)))

(if (not (eq ?save_exec t))
	then (close ?save_exec)
	else TRUE
)

)
