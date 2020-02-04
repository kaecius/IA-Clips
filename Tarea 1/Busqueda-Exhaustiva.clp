;http://www.comp.rgu.ac.uk/staff/smc/teaching/clips/vol1/vol1-Contents.html

(defglobal 
	?*ESTADO-INICIAL* = (create$ h1 aspiradora sucia h2 sucia)
	?*LISTA* = (create$ (implode$ ?*ESTADO-INICIAL*))
 	?*PADRE* = ?*ESTADO-INICIAL*
 	?*OPERADORES* = (create$ A I D)
 	?*SECUENCIA-DE-OPERADORES* = (create$)
 	?*VISITADOS* = (create$)
 	?*PASOS* = 8
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

(deffunction exito ($?estado)
  	(bind ?h1 (extrae-h1 ?estado))
  	(bind ?h2 (extrae-h2 ?estado))
	(and (habitacion-limpia? ?h1) (habitacion-limpia? ?h2))
)

(deffunction A ($?estado)
	(bind ?h1 (aspirar(extrae-h1 ?estado)))
	(bind ?h2 (aspirar(extrae-h2 ?estado)))
	(create$ ?h1 ?h2)
)

(deffunction I ($?estado)
	(bind ?h1 (poner-aspiradora(extrae-h1 ?estado)))
	(bind ?h2 (quitar-aspiradora(extrae-h2 ?estado)))
	(create$ ?h1 ?h2)
)

(deffunction D ($?estado)
	(bind ?h1 (quitar-aspiradora(extrae-h1 ?estado)))
	(bind ?h2 (poner-aspiradora(extrae-h2 ?estado)))
	(create$ ?h1 ?h2)
)

(deffunction prohibido? ($?estado)
	(eq $?estado PROHIBIDO)
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
		(if (not (prohibido? ?hijo)) then 
			(bind ?lista-operadores (create$ ?lista-operadores ?op))))
?lista-operadores)

(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (not (prohibido? ?hijo)) then 
			(bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo)))))
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
			(bind ?*LISTA* (create$ (hijos ?*PADRE*) ?*LISTA*)))
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
			(bind ?*LISTA* (create$ ?*LISTA* (hijos ?*PADRE*))))
		(bind ?i (+ ?i 1))
		(if (and (> ?*PASOS* 0) (= ?i ?*PASOS*)) then (break))
	)
	(if  (exito ?*PADRE*) 
		then (printout t "La solución es " ?*PADRE* crlf)
		else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
	)
)
