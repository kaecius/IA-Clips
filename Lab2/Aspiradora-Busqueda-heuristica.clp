(defglobal 
?*ESTADO-INICIAL* = (create$ h1 aspiradora sucia sucia  h2 sucia sucia sucia sucia cop )
 ?*OPERADORES* = (create$ A I D)
 ?*LISTA* = (create$   (implode$ ?*ESTADO-INICIAL*))
 ?*PADRE* = ?*ESTADO-INICIAL*
 ?*PASOS* = 0 
;indica que no hay l�mite en el n�mero de pasos
)

;;;EXPLICACION DE UN ESTADO ARBITRARIO
;sistema-aspiradora: lista (h1 aspiradora sucia h2 sucia cop I D I)
;h1 aspiradora sucia: indica que la aspiradora esta  y que la habitacion esta sucia
;h2 sucia: la habitacion de la derecha esta sucia y la aspiradora no esta  
;cop I D I: camino de operadores desde el estado inicial, ha ido a la izquierda, luego a la derecha y otra vez a la izquierda


(deffunction extrae-h1 ($?estado)
(bind ?posicion-h2 (member$ h2 ?estado))
(subseq$ ?estado 1 (- ?posicion-h2 1))
)

(deffunction extrae-h2 ($?estado)
(bind ?posicion-h2 (member$ h2 ?estado))
(bind ?posicion-cop (member$ cop ?estado))
(subseq$ ?estado  ?posicion-h2 (- ?posicion-cop 1))
)

;;Nueva funcion. En esta implementacion de la aspiradora se agrega la secuencia de operadores
;;utilizados para llegar al estado en el que se encuentra, por lo que esta funcion se encarga
;;de extraer esa secuencia de operadores a partir de separador cop.
(deffunction extrae-cop ($?estado)
(bind ?posicion-cop (member$ cop ?estado))
(bind ?ultima (length$ ?estado))
(subseq$ ?estado  ?posicion-cop ?ultima)
)

;;Esta funcion devuelve la lista del estado actual sin la secuencia de operadores utilizados,
;;es decir, elimina todo a partir de cop(incluido)
(deffunction estado-simple ($?estado)
(bind ?posicion-cop (member$ cop ?estado))
(subseq$ ?estado 1 (- ?posicion-cop 1))
)

(deffunction aspirar ($?habitacion)
(bind ?pos-aspiradora (member$ aspiradora ?habitacion))
(bind ?pos-sucia (member$ sucia ?habitacion))
(if (and ?pos-aspiradora ?pos-sucia) then 
(delete$ ?habitacion ?pos-sucia ?pos-sucia) else ?habitacion)
)

(deffunction quitar-aspiradora ($?habitacion)
(bind ?pos-aspiradora (member$ aspiradora ?habitacion))
(if ?pos-aspiradora  
	then (delete$ ?habitacion ?pos-aspiradora ?pos-aspiradora) 
	else ?habitacion)
)

(deffunction poner-aspiradora ($?habitacion)
(if (member$ aspiradora ?habitacion) then ?habitacion
else 
(create$ (first$ ?habitacion) aspiradora (rest$ ?habitacion))
)
)

(deffunction habitacion-limpia? ($?habitacion)
(not(member$ sucia ?habitacion))
)

;;Devuelve si no hay ningun elemento despues de la habitacion pasada como parametro, 
;;ningun elemento sucia ni aspiradora
(deffunction habitacion-vacia? ($?habitacion)
(and (not(member$ sucia ?habitacion))(not(member$ aspiradora ?habitacion)))
)

(deffunction exito ($?estado)
  (bind ?h1 (extrae-h1 ?estado))
  (bind ?h2 (extrae-h2 ?estado))
(and (habitacion-limpia? ?h1) (habitacion-limpia? ?h2)))


;;En las siguientes tres funciones el cambio es que se agrega 
;;el operador utilizado en la funcion a la lista A, I o D
(deffunction A ($?estado)
  (bind ?h1 (aspirar(extrae-h1 ?estado)))
  (bind ?h2 (aspirar(extrae-h2 ?estado)))
  (bind ?cop (extrae-cop ?estado))
  (create$ ?h1 ?h2 ?cop A))

(deffunction I ($?estado)
  (bind ?h1 (poner-aspiradora(extrae-h1 ?estado)))
  (bind ?h2 (quitar-aspiradora(extrae-h2 ?estado)))
  (bind ?cop (extrae-cop ?estado))
  (create$ ?h1 ?h2 ?cop I))

(deffunction D ($?estado)
  (bind ?h1 (quitar-aspiradora(extrae-h1 ?estado)))
  (bind ?h2 (poner-aspiradora(extrae-h2 ?estado)))
  (bind ?cop (extrae-cop ?estado))
  (create$ ?h1 ?h2 ?cop D))

(deffunction prohibido? ($?estado)
(eq $?estado PROHIBIDO)
)

;;heuristicos
;;Si la habitacion no esta limpia, se penaliza con un heuristico con mayor valor
(deffunction habitaciones-sucias ($?estado)
(bind ?h1 (extrae-h1 ?estado))
(bind ?h2 (extrae-h2 ?estado))
(bind ?n 0)
(if (not(habitacion-limpia? ?h1)) then (bind ?n (+ ?n 1)))
(if (not(habitacion-limpia? ?h2)) then (bind ?n (+ ?n 1)))
?n
)
;;Si la habitacion no esta vacia, tiene suciedad, por lo que como falta mas para llegar al estado
;;desado, el heuristico es mayor
(deffunction habitaciones-no-vacias ($?estado)
(bind ?h1 (extrae-h1 ?estado))
(bind ?h2 (extrae-h2 ?estado))
(bind ?n 0)
(if  (not(habitacion-vacia? ?h1)) then (bind ?n (+ ?n 1)))
(if (not(habitacion-vacia? ?h2)) then (bind ?n (+ ?n 1)))
?n
)

;;Minimo de los valores de la lista introducida como parametro
(deffunction minimum ($?v)
(if (=(length$ ?v) 1) then (nth$ 1 ?v) else 
(eval(format nil "(min %s)" (implode$ ?v)))
))

;;costes

(deffunction coste-1(?estado)
(length$ (rest$(extrae-cop ?estado)))
)

;;funcion constante cero

(deffunction cero (?estado) 0)


;;evaluacion de costes: definir funciones g, h y f=g+h, asi como las listas de valores 

(deffunction g ($?estado) (coste-1 ?estado))
(deffunction h ($?estado) (habitaciones-sucias ?estado))
(deffunction f ($?estado) (+(g ?estado) (h ?estado)))

;;la lista  ?*VALORES-f* contiene los valores f=g+h de cada estado de ?*LISTA

(defglobal 
 ?*VALORES-f* = (create$ (f ?*ESTADO-INICIAL*))
  ?*VALORES-g* = (create$ (g ?*ESTADO-INICIAL*))
   ?*VALORES-h* = (create$ (h ?*ESTADO-INICIAL*))
)

(deffunction valores-funcion-lista (?funcion $?lista)
(bind ?resultado-funcion (create$))
(progn$ (?elemento ?lista) 
	(bind ?estado (explode$ ?elemento))
	(bind ?resultado-funcion (create$ ?resultado-funcion (funcall ?funcion ?estado)))

)
?resultado-funcion)

;;;***************** funciones de búsqueda

(deffunction aplicar-operador (?operador $?estado)
(funcall  ?operador $?estado)
)
 
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
;;Busqueda que utiliza una funcion f=g+h para recorrer el espacio de estados, 
;;se elige el menor valor de f en cada paso para seguir con la busqueda
(deffunction busqueda_informada (?lista)
(bind ?i 1)
(while (and (not (= ?*PASOS* ?i))   (not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
	(printout t "Paso " ?i crlf)
	(bind ?min (minimum ?*VALORES-f*))
	(bind ?posmin (member$ ?min ?*VALORES-f*))
	(bind ?*PADRE*  (explode$ (nth$ ?posmin ?*LISTA*)))
	(printout t "Padre= " ?*PADRE* crlf)
	(printout t " g=" (nth ?posmin ?*VALORES-g*)
				" h=" (nth ?posmin ?*VALORES-h*)
				" f=g+h= " ?min crlf)
	(bind ?*LISTA*(delete$ ?*LISTA* ?posmin ?posmin))
	(bind ?*VALORES-f*(delete$ ?*VALORES-f* ?posmin ?posmin))
	(bind ?*VALORES-g*(delete$ ?*VALORES-g* ?posmin ?posmin))
	(bind ?*VALORES-h*(delete$ ?*VALORES-h* ?posmin ?posmin))
	(if (not (exito ?*PADRE*)) then
	        (bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?hijos   ?*LISTA*))
			(bind ?*VALORES-g* (create$(valores-funcion-lista g ?hijos) ?*VALORES-g*))
			(bind ?*VALORES-h* (create$(valores-funcion-lista h ?hijos) ?*VALORES-h*))
			(bind ?*VALORES-f* (create$(valores-funcion-lista f ?hijos) ?*VALORES-f*))
	(bind ?i (+ ?i 1))
)

(if  (exito ?*PADRE*) then (printout t "La solución es " ?*PADRE* crlf)
else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf)))
)
