;;documentaci�n de Clips 
;; https://www.csie.ntu.edu.tw/~sylee/courses/clips/bpg/top.html


;;;EXPLICACI�N DE UN ESTADO ARBITRARIO
; Un estado arbitrario es una lista (x y d cop op1... opk) donde 
;x es el contenido del primer recipiente, 
;y es el contenido del segundo 
;d es la cantidad de agua vertida por el desag�e
;cop es la abraviatura de "cadena de operadores" e indica que a continuaci�n 
;se guarda la cadena de operadores que lleva desde el estado inicial al actual

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
 ?*PASOS* = 0
;indica que no hay l�mite en el n�mero de pasos
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

(deffunction llenar-x ($?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else (if (= (nth$ 1 ?estado) ?*TX*) 
;		then PROHIBIDO
;		else 	
			(bind ?estado(replace$ ?estado 1 1  ?*TX*))
			(create$ ?estado llenar-x)
;		)
;	)
)
	
(deffunction llenar-y ($?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else (if (= (nth$ 2 ?estado) ?*TY*) 
;		then PROHIBIDO
;		else 	
		(bind ?estado(replace$ ?estado 2 2  ?*TY*))
		(create$ ?estado llenar-y)
;	      )
;	)
)


(deffunction volcar-x-y ($?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else 	
	(bind ?cop (extrae-cop ?estado))
	(bind ?x (cantidad-x ?estado))
	(bind ?y (cantidad-y ?estado))
	(bind ?falta-y (- ?*TY* ?y)) ;lo que falta para llenar y
	;Si lo que hay en x es <= que lo que falta para llenar y
        ;entonces (x'=0, y'=x+y)
	(if (<= ?x ?falta-y) 
		then 

		(bind ?nvo (create$ 0 (+ ?x ?y)))
		else
		;lo que hay en x es m�s que lo que falta para llenar y
		;entonces (x'=x-falta-y, y'=*TY*)
		(bind ?nvo(create$ (- ?x ?falta-y) ?*TY*))
	)
       (create$ ?nvo ?cop volcar-x-y)
;)
)


(deffunction volcar-y-x ($?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else 	
	(bind ?cop (extrae-cop ?estado))
	(bind ?x (cantidad-x ?estado))
	(bind ?y (cantidad-y ?estado))
	(bind ?falta-x (- ?*TX* ?x)) ;lo que falta para llenar x
	;Si lo que hay en y es <= que lo que falta para llenar x
        ;entonces (x'=x+y, y'=0)
	(if (<= ?y ?falta-x) 
		then 
		(bind ?nvo(create$ (+ ?x ?y) 0))
		else
		;lo que hay en y es m�s que lo que falta para llenar x
		;entonces (x'=*TX* , y'=y - falta-x)
		(bind ?nvo(create$  ?*TX* (- ?y ?falta-x)))
	)
	       (create$ ?nvo ?cop volcar-y-x)
;)
)


(deffunction tirar-x(?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else
  (bind ?nvo (create$ 0 (cantidad-y ?estado)))
  (create$ ?nvo (extrae-cop ?estado) tirar-x)
;)
)

(deffunction tirar-y(?estado)
;(if (eq ?estado PROHIBIDO) 
;	then PROHIBIDO
;	else
  (bind ?nvo (create$  (cantidad-x ?estado) 0))
  (create$ ?nvo (extrae-cop ?estado) tirar-y)
;)
)
				

(deffunction prohibido? ($?estado)
  (eq $?estado PROHIBIDO))

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

;;Funci�n de coste uniforme 1 (longitud del camino)

(deffunction coste-1(?estado)
(length$ (rest$(extrae-cop ?estado)))
)


;;;funcion constante cero

(deffunction cero (?estado) 0)


;;;evaluaci�n de costes: definir funciones g, h y f=g+h, as� como las listas de valores 
;
(deffunction g ($?estado) (cero ?estado))
(deffunction h ($?estado) (cero ?estado))
(deffunction f ($?estado) (+(g ?estado) (h ?estado)))


;;;;;;;;;;;B�squeda guiada por informaci�n, funciones auxiliares

;;;la lista  ?*VALORES-f* contiene los valores f=g+h de cada estado de ?*LISTA






;;C�lculo de los valores de las funciones f, g, h (se pasan como par�metro) para toda la ?*LISTA* estados (como strings)
(deffunction valores-funcion-lista (?funcion $?lista)
(bind ?resultado-funcion (create$))
(progn$ (?elemento ?lista) 
	(bind ?estado (explode$ ?elemento))
	(bind ?resultado-funcion (create$ ?resultado-funcion (funcall ?funcion ?estado)))

)
?resultado-funcion)


;;;;***************** funciones de b�squeda
;
(deffunction aplicar-operador (?operador $?estado)
(funcall  ?operador $?estado)
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
))


;;esta funci�n busca el m�nimo de una lista incluso si tiene menos de 2 elementos
(deffunction minimum ($?v)
(if (=(length$ ?v) 1) then (nth$ 1 ?v) else 
(eval(format nil "(min %s)" (implode$ ?v)))
))

(defglobal
?*VALORES-f* = (create$ (f ?*ESTADO-INICIAL*))
 ?*VALORES-g* = (create$ (g ?*ESTADO-INICIAL*))
  ?*VALORES-h* = (create$ (h ?*ESTADO-INICIAL*))
  ?*MIN* = (nth$ 1 ?*VALORES-f*)
  ?*POSMIN* = 1
  ?*ULTIMO-ESTADO* = (implode$ (estado-actual ?*PADRE*))
  ?*VISITADOS* = (create$)
)	







;;;;;;;;;;;***********BUSQUEDA INFORMADA*****************

(deffunction busqueda_informada_con_visitados (?lista)
(bind ?i 1)
(while (and (not (= ?*PASOS* ?i))   (not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
	
;contador de n�mero de pasos
(printout t "Paso " ?i crlf)

   (while (member$  ?*ULTIMO-ESTADO* ?*VISITADOS*) 

;borrar el padre de la posici�n ?*POSMIN* y los valores de esa posici�n en todas las listas
	(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-f*(delete$ ?*VALORES-f* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
(if (eq ?*LISTA* (create$)) then (break))
;buscar el nuevo padre
	(bind ?*MIN* (minimum ?*VALORES-f*))
	(bind ?*POSMIN* (member$ ?*MIN* ?*VALORES-f*))
	(bind ?*PADRE*  (explode$ (nth 1 ?*LISTA*)))
;Ahora obtenemos el estado visitado simplemente quitando el camino de operadores
	(bind   ?*ULTIMO-ESTADO* (implode$(estado-actual ?*PADRE*)))
;con el bucle as� organizado el contador ?i no avanza

	)


(if (not(eq ?*LISTA* (create$))) then 

 ;Ahora tenemos la seguridad de que al salir del bucle, el �ltimo-estado no ha sido visitado
;lo metemos en visitados
	(bind ?*VISITADOS* (create$ ?*VISITADOS* ?*ULTIMO-ESTADO* ))

;Imprimimos los mensajes para saber valores de f=g+h
	
	(printout t "Padre= " ?*PADRE* crlf)
	(printout t " g=" (nth ?*POSMIN* ?*VALORES-g*)
				" h=" (nth ?*POSMIN* ?*VALORES-h*)
				" f=g+h= " ?*MIN* crlf)
				
;Borramos ahora el padre no visitado y los valores f=g+h
	(bind ?*LISTA*(delete$ ?*LISTA* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-f*(delete$ ?*VALORES-f* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?*POSMIN* ?*POSMIN*))
	(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?*POSMIN* ?*POSMIN*))
;Incluimos sus hijos y los valores f=g+h en las correspondientes listas, en profundidad
	(if (not (exito ?*PADRE*)) then
	        (bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?hijos   ?*LISTA*))
			(bind ?*VALORES-g* (create$(valores-funcion-lista g ?hijos) ?*VALORES-g*))
			(bind ?*VALORES-h* (create$(valores-funcion-lista h ?hijos) ?*VALORES-h*))
			(bind ?*VALORES-f* (create$(valores-funcion-lista f ?hijos) ?*VALORES-f*))
;incrementamos el contador
(printout t "?*VISITADOS*=" ?*VISITADOS* crlf)
(printout t " ?*LISTA*=" ?*LISTA*  crlf )
	(bind ?i (+ ?i 1))
)
)
)
;;Salimos del bucle porque el padre sea �xito o porque no haya soluci�n
(if  (exito ?*PADRE*) then (printout t "La soluci�n es " ?*PADRE* crlf)
else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay soluci�n" crlf)))
)

