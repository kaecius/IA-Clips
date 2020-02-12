;http://www.comp.rgu.ac.uk/staff/smc/teaching/clips/vol1/vol1-Contents.html

(defglobal 
	?*ESTADO-INICIAL* = (create$ h1 aspiradora sucia sucia h2 sucia sucia sucia)
	?*LISTA* = (create$ (implode$ ?*ESTADO-INICIAL*))
 	?*PADRE* = ?*ESTADO-INICIAL*
 	?*OPERADORES* = (create$ A I D)
 	?*SECUENCIA-DE-OPERADORES* = (create$)
 	?*VISITADOS* = (create$)
 	?*PASOS* = 100
	?*GRAPH-FILE* = graph.gv
)

(deffunction print-edge ($?padre) ($?hijo) ($?op)
	(format ?*GRAPH-FILE* "%s -> %s [label=\"%s\"];\n" (implode$ $?padre) (implode$ $?hijo) ?op)
)

;sistema-aspiradora: lista (h1 aspiradora sucia h2 sucia)

(deffunction extrae-h1 ($?estado)
	(bind ?posicion-h2 (member$ h2 ?estado))
	(subseq$ ?estado 1 (- ?posicion-h2 1))
)

(deffunction extrae-h2 ($?estado)
	(bind ?posicion-h2 (member$ h2 ?estado))
	(bind ?ultima (length$ ?estado))
	(subseq$ ?estado  ?posicion-h2 ?ultima)
)

(deffunction aspirar ($?habitacion)
	(bind ?pos-aspiradora (member$ aspiradora ?habitacion))
	(bind ?pos-sucia (member$ sucia ?habitacion))
	(if (and ?pos-aspiradora ?pos-sucia) 
		then (delete$ ?habitacion ?pos-sucia ?pos-sucia) 
		else ?habitacion)
)

(deffunction quitar-aspiradora ($?habitacion)
	(bind ?pos-aspiradora (member$ aspiradora ?habitacion))
	(if ?pos-aspiradora  
		then (delete$ ?habitacion ?pos-aspiradora ?pos-aspiradora) 
		else ?habitacion)
)

(deffunction poner-aspiradora ($?habitacion)
	(if (member$ aspiradora ?habitacion) 
		then ?habitacion
		else (create$ (first$ ?habitacion) aspiradora (rest$ ?habitacion)))
)

(deffunction habitacion-limpia? ($?habitacion)
	(not(member$ sucia ?habitacion))
)
;Comprueba si la aspiradora esta en la habitacion
(deffunction esta-aspiradora? ($?habitacion)
	(not (eq (member$ aspiradora $?habitacion) FALSE))
)

(deffunction exito ($?estado)
  	(bind ?h1 (extrae-h1 ?estado))
  	(bind ?h2 (extrae-h2 ?estado))
	(and (habitacion-limpia? ?h1) (habitacion-limpia? ?h2))
)

(deffunction A ($?estado)
	(bind ?aspirado FALSE)
	(bind ?h1 (extrae-h1 ?estado))
	(bind ?h2 (extrae-h2 ?estado))

	(if (and (not (habitacion-limpia? ?h1)) (esta-aspiradora? ?h1)) 
		then (bind ?h1 (aspirar ?h1)) 
			 (bind ?aspirado TRUE)
	)

	(if (and (not (habitacion-limpia? ?h2)) (esta-aspiradora? ?h2))
		then (bind ?h2 (aspirar ?h2)) 
			 (bind ?aspirado TRUE)
	)

	(if (not ?aspirado)
		then PROHIBIDO
		else (create$ ?h1 ?h2)
	)
)

(deffunction I ($?estado)
	(bind ?h1 (extrae-h1 ?estado))
	
	(if (esta-aspiradora? ?h1) 
		then PROHIBIDO
		else (bind ?h1 (poner-aspiradora(extrae-h1 ?estado)))
			 (bind ?h2 (quitar-aspiradora(extrae-h2 ?estado)))
			 (create$ ?h1 ?h2)
	)
)

(deffunction D ($?estado)
	(bind ?h2 (extrae-h2 ?estado))
	
	(if (esta-aspiradora? ?h2) 
		then PROHIBIDO
		else (bind ?h1 (quitar-aspiradora(extrae-h1 ?estado)))
			 (bind ?h2 (poner-aspiradora(extrae-h2 ?estado)))
			 (create$ ?h1 ?h2)
			 
	)
)

(deffunction prohibido? ($?estado)
	(eq $?estado (create$ PROHIBIDO))
)

(deffunction aplicar-operador (?operador $?estado)
	(eval
		(format nil "( %s (create$ %s))" ?operador (implode$ ?estado))
	)
)

(deffunction operadores-hijos($?estado)
	(bind $?lista-operadores (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (not (prohibido? ?hijo)) 
			then (bind ?lista-operadores (create$ ?lista-operadores ?op))))
	?lista-operadores)

(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (not (prohibido? ?hijo)) 
			then (bind ?lista-hijos (create$ ?lista-hijos (implode$ ?hijo))))
	)
	;si la ultima instruccion a ejecutar es el if sin entrar devuelve un false por lo que casca
	$?lista-hijos 
)

(deffunction busqueda-en-profundidad ($?lista)
	(bind ?i 0)
	(while (and(not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
		(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1  ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then 
			(bind ?operadores-hijos (operadores-hijos ?*PADRE*))
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
		(bind ?*PADRE*  (explode$(nth$ 1  ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then 
			(bind ?operadores-hijos (operadores-hijos ?*PADRE*))
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

(deffunction busqueda-en-profundidad-arbol ($?lista)
	(printout t "Introduzca el nombre del fichero en el que se escribirá el árbol: ")
	(bind ?file (read))
	(open ?file ?*GRAPH-FILE* "a")
	(bind ?i 0)
	(while (and(not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
		(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1  ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then 
			(bind ?operadores-hijos (operadores-hijos ?*PADRE*))
			(bind ?hijos (hijos ?*PADRE*))
			(bind ?*LISTA* (create$ ?hijos ?*LISTA*)))
		(bind ?i (+ ?i 1))
		(if (and (> ?*PASOS* 0) (= ?i ?*PASOS*)) then (break))
	)

	(if  (exito ?*PADRE*) 
		then (printout t "La solución es " ?*PADRE* crlf)
		else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
	)

	(close ?*GRAPH-FILE*)
)
