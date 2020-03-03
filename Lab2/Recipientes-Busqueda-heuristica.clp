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
 ?*LISTA* = (create$ (implode$ ?*ESTADO-INICIAL*))
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

;;;heuristicos

;
;;;heur�stico dado en el examen

(deffunction h-examen(?estado)
  (abs(-(+ (cantidad-x ?estado) (cantidad-y ?estado)) 2)))

;;;heurístico dado en el examen modificado para ser admisible
(deffunction h-examen-admisible (?estado)
	(/ (h-examen ?estado) 5)
)

;;;costes

;;Funci�n de coste uniforme 1 (longitud del camino)

(deffunction coste-1(?estado)
(length$ (rest$(extrae-cop ?estado)))
)

;;;funcion constante cero

(deffunction cero (?estado) 0)

;;;Función de coste examen 2018-2019

(deffunction coste-camino (?estado)

	(bind ?estado_actual (create$ 0 0 cop))
	(bind ?g 0)

	(progn$ (?op (rest$(extrae-cop ?estado)))
		(bind ?estado_actual (funcall ?op ?estado_actual))
		(bind ?g (+ ?g (+ (cantidad-x ?estado_actual) (cantidad-y ?estado_actual))))
	)
	?g
)

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

(deffunction minimum-floats($?v)
	(if (=(length$ ?v) 1) 
		then (nth$ 1 ?v)
		else
		(bind ?minf "(min")
		(progn$ (?x ?v) (bind ?minf (str-cat ?minf (if (integerp ?x)
														then (format nil " %d"  ?x)
														else (format nil " %f"  ?x)
									))))	
		(bind ?minf (str-cat ?minf ")")) 
		(eval ?minf)
	)
)

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

(deffunction hill_climbing_v1 (?h $?save_tree)

	(bind ?i 1)

	(bind ?*FUNCION-h* (format nil "(funcall %s " ?h))
	(bind ?*FUNCION-h* (str-cat ?*FUNCION-h* "(create$ %s))"))
	(bind ?*VALORES-h* (create$ (h (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*CON-PROHIBIDO TRUE)
	(bind ?*MIN* (minimum ?*VALORES-h*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
	(bind ?h_anterior 500000) ;Se inicializa con un valor muy grande
	
	(if (nth 1 ?save_tree)
		then (printout t "Introduzca el nombre del fichero en el que se guardará el árbol en formato dot: ")
			 (bind ?filename (read))
			 (open ?filename graph.dot "a")
			 (format graph.dot "digraph G {%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n    n0 -> \"%s\";%n" (implode$ (estado-actual ?*PADRE*)))
			 (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none, fillcolor=yellow, style=filled];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
	)

	(bind ?no_solution FALSE)

	(while (not (eq ?*LISTA* (create$))) do
	
		(while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) do

			(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
			
			(if (eq ?*LISTA* (create$)) then (bind ?no_solution TRUE) (break))
			
			(bind ?*MIN* (minimum ?*VALORES-h*))
			(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
			(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
			(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))

		)

		(if (>= ?*MIN* ?h_anterior)
			then (bind ?no_solution TRUE) (break)
		)

		(if (not (= 1 ?i))
			then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
		)

		(if (not (eq ?*LISTA* (create$)))
			then (printout t crlf "-------Paso " ?i "-------" crlf "Padre = " ?*PADRE* tab "h = " (nth ?*POSMIN* ?*VALORES-h*) crlf)

				 (if (exito ?*PADRE*) then (break))

				 (bind ?hijos (hijos ?*PADRE*))

				 (progn$ (?hijo ?hijos)
				 	 (bind ?estado_hijo (implode$ (estado-actual (explode$ ?hijo))))
				 	 (if (not (member$ ?estado_hijo ?*VISITADOS*))
					 	then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD> </TD></TR></TABLE>>, shape=none];%n" ?estado_hijo ?estado_hijo (h ?hijo))
					 )
					 (format graph.dot "    \"%s\" -> \"%s\" [label=\"%s\"];%n" ?*ULTIMO-ESTADO* ?estado_hijo (nth (length$ (explode$ ?hijo)) (explode$ ?hijo)))
				 )

				 (bind ?*LISTA* ?hijos)
				 (bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO*))
				 (bind ?*VALORES-h* (valores-funcion-lista h ?hijos))
				 (bind ?h_anterior ?*MIN*)
				 (bind ?*MIN* (minimum ?*VALORES-h*))
				 (bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
				 (bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
				 (bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))
		)

		(bind ?i (+ ?i 1))
	
	)

	(if ?no_solution
		then (printout t "No hay solución." crlf)
		else (printout t "La solución es " ?*PADRE* crlf)
			 (format graph.dot "    \"%s\" [fillcolor=green, style=filled];%n" ?*ULTIMO-ESTADO*)
	)

	(printout graph.dot "}" crlf)
	(close graph.dot)

)

(deffunction hill_climbing_v2 (?h $?save_tree)
(bind ?i 1)

	(bind ?*FUNCION-h* (format nil "(funcall %s " ?h))
	(bind ?*FUNCION-h* (str-cat ?*FUNCION-h* "(create$ %s))"))
	(bind ?*VALORES-h* (create$ (h (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*CON-PROHIBIDO TRUE)
	(bind ?*MIN* (minimum ?*VALORES-h*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
	
	(if (nth 1 ?save_tree)
		then (printout t "Introduzca el nombre del fichero en el que se guardará el árbol en formato dot: ")
			 (bind ?filename (read))
			 (open ?filename graph.dot "a")
			 (format graph.dot "digraph G {%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n    n0 -> \"%s\";%n" (implode$ (estado-actual ?*PADRE*)))
			 (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none, fillcolor=yellow, style=filled];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
	)

	(bind ?no_solution FALSE)

	(while (not (eq ?*LISTA* (create$))) do
	
		(while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) do

			(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
			
			(if (eq ?*LISTA* (create$)) then (bind ?no_solution TRUE) (break))
			
			(bind ?*MIN* (minimum ?*VALORES-h*))
			(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
			(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
			(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))

		)

		(if (not (= 1 ?i))
			then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
		)

		(if (not (eq ?*LISTA* (create$)))
			then (printout t crlf "-------Paso " ?i "-------" crlf "Padre = " ?*PADRE* tab "h = " (nth ?*POSMIN* ?*VALORES-h*) crlf)

				 (if (exito ?*PADRE*) then (break))

				 (bind ?hijos (hijos ?*PADRE*))

				 (progn$ (?hijo ?hijos)
				 	 (bind ?estado_hijo (implode$ (estado-actual (explode$ ?hijo))))
				 	 (if (not (member$ ?estado_hijo ?*VISITADOS*))
					 	then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD> </TD></TR></TABLE>>, shape=none];%n" ?estado_hijo ?estado_hijo (h ?hijo))
					 )
					 (format graph.dot "    \"%s\" -> \"%s\" [label=\"%s\"];%n" ?*ULTIMO-ESTADO* ?estado_hijo (nth (length$ (explode$ ?hijo)) (explode$ ?hijo)))
				 )

				 (bind ?*LISTA* ?hijos)
				 (bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO*))
				 (bind ?*VALORES-h* (valores-funcion-lista h ?hijos))
				 (bind ?*MIN* (minimum ?*VALORES-h*))
				 (bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
				 (bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
				 (bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))
		)

		(bind ?i (+ ?i 1))
	
	)

	(if ?no_solution
		then (printout t "No hay solución." crlf)
		else (printout t "La solución es " ?*PADRE* crlf)
			 (format graph.dot "    \"%s\" [fillcolor=green, style=filled];%n" ?*ULTIMO-ESTADO*)
	)

	(printout graph.dot "}" crlf)
	(close graph.dot)

)

(deffunction best-first (?h $?save_tree)

	(bind ?i 1)

	(bind ?*FUNCION-h* (format nil "(funcall %s " ?h))
	(bind ?*FUNCION-h* (str-cat ?*FUNCION-h* "(create$ %s))"))
	(bind ?*VALORES-h* (create$ (h (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*CON-PROHIBIDO TRUE)
	(bind ?*MIN* (minimum ?*VALORES-h*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
	
	(if (nth 1 ?save_tree)
		then (printout t "Introduzca el nombre del fichero en el que se guardará el árbol en formato dot: ")
			 (bind ?filename (read))
			 (open ?filename graph.dot "a")
			 (format graph.dot "digraph G {%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n    n0 -> \"%s\";%n" (implode$ (estado-actual ?*PADRE*)))
			 (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none, fillcolor=yellow, style=filled];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
	)

	(while (not (eq ?*LISTA* (create$))) do
	
		(while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) do

			(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
			
			(if (eq ?*LISTA* (create$)) then (break))
			
			(bind ?*MIN* (minimum ?*VALORES-h*))
			(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-h*))
			(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
			(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))

		)

		(if (not (= 1 ?i))
			then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
		)

		(if (not (eq ?*LISTA* (create$)))
			then (printout t crlf "-------Paso " ?i "-------" crlf "Padre = " ?*PADRE* tab "h = " (nth ?*POSMIN* ?*VALORES-h*) crlf)

				 (if (exito ?*PADRE*) then (break))

				 (bind ?hijos (hijos ?*PADRE*))

				 (progn$ (?hijo ?hijos)
				 	 (bind ?estado_hijo (implode$ (estado-actual (explode$ ?hijo))))
				 	 (if (not (member$ ?estado_hijo ?*VISITADOS*)) ;Just not to erase second TD's content (order in which the node has been visited)
					 	then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">h = %d</FONT></TD><TD> </TD></TR></TABLE>>, shape=none];%n" ?estado_hijo ?estado_hijo (h ?hijo))
					 )
					 (format graph.dot "    \"%s\" -> \"%s\" [label=\"%s\"];%n" ?*ULTIMO-ESTADO* ?estado_hijo (nth (length$ (explode$ ?hijo)) (explode$ ?hijo)))
				 )

				 (bind ?*LISTA* (create$ ?hijos ?*LISTA*))
				 (bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO*))
				 (bind ?*VALORES-h* (create$ (valores-funcion-lista h ?hijos) ?*VALORES-h*))
				 (bind ?*POSMIN* (+ ?*POSMIN* (length$ ?hijos)))
		)

		(bind ?i (+ ?i 1))
	
	)

	(if (eq ?*LISTA* (create$))
		then (printout t "No hay solución." crlf)
		else (printout t "La solución es " ?*PADRE* crlf)
			 (format graph.dot "    \"%s\" [fillcolor=green, style=filled];%n" ?*ULTIMO-ESTADO*)
	)

	(printout graph.dot "}" crlf)
	(close graph.dot)

)

(deffunction branch_and_bound_v1 (?g $?save_tree)

	(bind ?i 1)

	(bind ?*FUNCION-g* (format nil "(funcall %s " ?g))
	(bind ?*FUNCION-g* (str-cat ?*FUNCION-g* "(create$ %s))"))
	(bind ?*VALORES-g* (create$ (g (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*CON-PROHIBIDO TRUE)
	(bind ?*MIN* (minimum ?*VALORES-g*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-g*))
	
	(if (nth 1 ?save_tree)
		then (printout t "Introduzca el nombre del fichero en el que se guardará el árbol en formato dot: ")
			 (bind ?filename (read))
			 (open ?filename graph.dot "a")
			 (format graph.dot "digraph G {%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n    n0 -> \"%s\";%n" (implode$ (estado-actual ?*PADRE*)))
			 (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none, fillcolor=yellow, style=filled];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
	)

	(while (not (eq ?*LISTA* (create$))) do
	
		(while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) do

			(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?*POSMIN* ?*POSMIN*))
			
			(if (eq ?*LISTA* (create$)) then (break))
			
			(bind ?*MIN* (minimum ?*VALORES-g*))
			(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-g*))
			(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
			(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))

		)

		(if (not (= 1 ?i))
			then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
		)

		(if (not (eq ?*LISTA* (create$)))
			then (printout t crlf "-------Paso " ?i "-------" crlf "Padre = " ?*PADRE* tab "f = g = " (nth ?*POSMIN* ?*VALORES-g*) crlf)

				 (if (exito ?*PADRE*) then (break))

				 (bind ?hijos (hijos ?*PADRE*))

				 (progn$ (?hijo ?hijos)
				 	 (bind ?estado_hijo (implode$ (estado-actual (explode$ ?hijo))))
				 	 (if (not (member$ ?estado_hijo ?*VISITADOS*)) ;Just not to erase second TD's content (order in which the node has been visited)
					 	then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g = %d</FONT></TD><TD> </TD></TR></TABLE>>, shape=none];%n" ?estado_hijo ?estado_hijo (g ?hijo))
					 )
					 (format graph.dot "    \"%s\" -> \"%s\" [label=\"%s\"];%n" ?*ULTIMO-ESTADO* ?estado_hijo (nth (length$ (explode$ ?hijo)) (explode$ ?hijo)))
				 )

				 (bind ?*LISTA* (create$ ?hijos ?*LISTA*))
				 (bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO*))
				 (bind ?*VALORES-g* (create$ (valores-funcion-lista g ?hijos) ?*VALORES-g*))
				 (bind ?*POSMIN* (+ ?*POSMIN* (length$ ?hijos)))
		)

		(bind ?i (+ ?i 1))
	
	)

	(if (eq ?*LISTA* (create$))
		then (printout t "No hay solución." crlf)
		else (printout t "La solución es " ?*PADRE* crlf)
			 (format graph.dot "    \"%s\" [fillcolor=green, style=filled];%n" ?*ULTIMO-ESTADO*)
	)

	(printout graph.dot "}" crlf)
	(close graph.dot)

)

(deffunction branch_and_bound_v2 (?g ?h $?save_tree)

	(bind ?i 1)

	(bind ?*FUNCION-g* (format nil "(funcall %s " ?g))
	(bind ?*FUNCION-g* (str-cat ?*FUNCION-g* "(create$ %s))"))
	(bind ?*VALORES-g* (create$ (g (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*FUNCION-h* (format nil "(funcall %s " ?h))
	(bind ?*FUNCION-h* (str-cat ?*FUNCION-h* "(create$ %s))"))
	(bind ?*VALORES-h* (create$ (h (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*VALORES-f* (create$ (f (implode$ ?*ESTADO-INICIAL*))))
	(bind ?*CON-PROHIBIDO TRUE)
	(bind ?*MIN* (minimum-floats ?*VALORES-g*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-g*))
	
	(if (nth 1 ?save_tree)
		then (printout t "Introduzca el nombre del fichero en el que se guardará el árbol en formato dot: ")
			 (bind ?filename (read))
			 (open ?filename graph.dot "a")
			 (format graph.dot "digraph G {%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n    n0 -> \"%s\";%n" (implode$ (estado-actual ?*PADRE*)))
			 (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g + h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none, fillcolor=yellow, style=filled];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
	)

	(while (not (eq ?*LISTA* (create$))) do
	
		(while (member$ ?*ULTIMO-ESTADO* ?*VISITADOS*) do

			(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
			(bind ?*VALORES-f*(delete$ ?*VALORES-f* ?*POSMIN* ?*POSMIN*))
			
			(if (eq ?*LISTA* (create$)) then (break))
			
			;FIXME: crashes because of floats :/
			(bind ?*MIN* (minimum-floats ?*VALORES-f*))
			(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-f*))
			(bind ?*PADRE*  (explode$ (nth ?*POSMIN* ?*LISTA*)))
			(bind ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))

		)

		(if (not (= 1 ?i))
			then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g + h = %d</FONT></TD><TD>%d</TD></TR></TABLE>>, shape=none];%n" ?*ULTIMO-ESTADO* ?*ULTIMO-ESTADO* ?*MIN* ?i)
		)

		(if (not (eq ?*LISTA* (create$)))
			then (printout t crlf "-------Paso " ?i "-------" crlf "Padre = " ?*PADRE* tab "f = g + h = " (nth ?*POSMIN* ?*VALORES-g*) crlf)

				 (if (exito ?*PADRE*) then (break))

				 (bind ?hijos (hijos ?*PADRE*))

				 (progn$ (?hijo ?hijos)
				 	 (bind ?estado_hijo (implode$ (estado-actual (explode$ ?hijo))))
				 	 (if (not (member$ ?estado_hijo ?*VISITADOS*)) ;Just not to erase second TD's content (order in which the node has been visited)
					 	then (format graph.dot "    \"%s\" [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\"><TR><TD>%s<BR/><FONT POINT-SIZE=\"10\">f = g + h = %d</FONT></TD><TD> </TD></TR></TABLE>>, shape=none];%n" ?estado_hijo ?estado_hijo (g ?hijo))
					 )
					 (format graph.dot "    \"%s\" -> \"%s\" [label=\"%s\"];%n" ?*ULTIMO-ESTADO* ?estado_hijo (nth (length$ (explode$ ?hijo)) (explode$ ?hijo)))
				 )

				 (bind ?*LISTA* (create$ ?hijos ?*LISTA*))
				 (bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO*))
				 (bind ?*VALORES-g* (create$ (valores-funcion-lista g ?hijos) ?*VALORES-g*))
				 (bind ?*VALORES-h* (create$ (valores-funcion-lista h ?hijos) ?*VALORES-h*))
				 (bind ?*VALORES-f* (create$ (valores-funcion-lista f ?hijos) ?*VALORES-f*))
				 (bind ?*POSMIN* (+ ?*POSMIN* (length$ ?hijos)))
		)

		(bind ?i (+ ?i 1))
	
	)

	(if (eq ?*LISTA* (create$))
		then (printout t "No hay solución." crlf)
		else (printout t "La solución es " ?*PADRE* crlf)
			 (format graph.dot "    \"%s\" [fillcolor=green, style=filled];%n" ?*ULTIMO-ESTADO*)
	)

	(printout graph.dot "}" crlf)
	(close graph.dot)

)


;(load Recipientes-Busqueda-heuristica.clp)
; (progn$ (?x (create$ 1 2 3 4.5)) (str-cat ?minf (format nil " %f"  ?x)))