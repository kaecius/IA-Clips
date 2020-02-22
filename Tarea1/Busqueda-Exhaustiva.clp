(defglobal 
	?*ESTADO-INICIAL* = (create$ h1 sucia sucia h2 aspiradora sucia sucia sucia)
	?*LISTA* = (create$ (implode$ ?*ESTADO-INICIAL*))
 	?*PADRE* = ?*ESTADO-INICIAL*
 	?*OPERADORES* = (create$ A I D)
 	?*LISTA-DE-OPERADORES* = (create$)
 	?*VISITADOS* = (create$)
 	?*PASOS* = 10
	?*GRAPH-FILE* = graph.dot
	?*CON-PROHIBIDO* = TRUE
	?*CON-VISITADOS* = TRUE
	?*VISITADOS* = (create$ ?*ESTADO-INICIAL*)
)

;Crea una transicion de estado padre a estado hijo con el operador en formato graphviz
(deffunction print-edge (?padre ?hijo $?op)
	(format ?*GRAPH-FILE* "    %s -> %s [label=\"%s\"];%n" ?padre ?hijo (implode$ $?op))
)

;Convierte los espacios en '_'
(deffunction state-to-str (?estado)
   (bind ?res "")
   (bind ?i (str-index " " ?estado))
   (while ?i
      (bind ?res (str-cat ?res (sub-string 1 (- ?i 1) ?estado) _))
      (bind ?estado (sub-string (+ ?i 1) (str-length ?estado) ?estado))
      (bind ?i (str-index " " ?estado)))
   (bind ?res (str-cat ?res ?estado))
)

;Obtiene la información de la habitación 1 del estado eg:(h1 sucia sucia)
(deffunction extrae-h1 ($?estado)
	(bind ?posicion-h2 (member$ h2 ?estado)) ;Busca la posición de la habitación dos (h2)
	(subseq$ ?estado 1 (- ?posicion-h2 1)) ;Obtiene la sublista desde la posición 1 hasta la habitación 2 (h2) sin incluir
)

;Obtiene la información de la habitación 2 del estado
(deffunction extrae-h2 ($?estado)
	(bind ?posicion-h2 (member$ h2 ?estado)) ;Busca la posición de la habitación dos (h2)
	(bind ?ultima (length$ ?estado)) ;obtiene el tamaño de la lista estado
	(subseq$ ?estado  ?posicion-h2 ?ultima) ;Devuelve la sublista desde la posición de h2 hasta la última
)

;Elimina una suciedad (sucia) de la habitación
(deffunction aspirar ($?habitacion)
	(bind ?pos-aspiradora (member$ aspiradora ?habitacion)) ;obtiene si la aspiradora está o no en la habitación, buscando su index en la lista
	(bind ?pos-sucia (member$ sucia ?habitacion)) ;obtiene la posición del primer sucia en la habitación
	(if (and ?pos-aspiradora ?pos-sucia) ;si se ha encontrado tanto la aspiradora como alguna suciedad
		then (delete$ ?habitacion ?pos-sucia ?pos-sucia) ; elimina la suciedad encontrada de la habitación eg: (h1 aspiradora sucia) -> (h1 aspiradora)
		else ?habitacion)
)

;Quita la aspiradora de la habitación
(deffunction quitar-aspiradora ($?habitacion)
	(bind ?pos-aspiradora (member$ aspiradora ?habitacion)) ;Busca la posición de la aspiradora en la lista de la habitación
	(if ?pos-aspiradora ;si se encuentra la aspiradora
		then (delete$ ?habitacion ?pos-aspiradora ?pos-aspiradora) ;quita la aspiradora de la habitación
		else ?habitacion)
)

;Pone la aspiradora en la habitación
(deffunction poner-aspiradora ($?habitacion)
	(if (member$ aspiradora ?habitacion)  
		then ?habitacion 
		else (create$ (first$ ?habitacion) aspiradora (rest$ ?habitacion))) ; si no, crea una nueva lista con la aspiradora eg: (h1 sucia) -> (h1 aspiradora sucia)
)

;Devuelve si la habitación no tiene suciedad (sucia)
(deffunction habitacion-limpia? ($?habitacion)
	(not(member$ sucia ?habitacion))
)

;Comprueba si la aspiradora esta en la habitacion
(deffunction esta-aspiradora? ($?habitacion)
	(not (eq (member$ aspiradora $?habitacion) FALSE))
)

;Comprueba si se ha llegado a un estado de exito
(deffunction exito ($?estado)
  	(bind ?h1 (extrae-h1 ?estado))
  	(bind ?h2 (extrae-h2 ?estado))
	(and (habitacion-limpia? ?h1) (habitacion-limpia? ?h2)) ;si las dos estan limpias se ha llegado a un estado de exito
)

;Operador Aspirar
(deffunction A ($?estado)
	(bind ?aspirado FALSE) ; Variable que se activa si se ha aspirado alguna habitación
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

	(if (and (not ?aspirado) ?*CON-PROHIBIDO*) ; Si no se ha aspirado y están activados los estados prohibidos
		then PROHIBIDO
		else (create$ ?h1 ?h2)
	)
)

;Operador Izquierda
(deffunction I ($?estado)
	(bind ?h1 (extrae-h1 ?estado))
	
	(if (and (esta-aspiradora? ?h1) ?*CON-PROHIBIDO*)  ;Si ya está la aspiradora en la izquierda y están activas los estados prohibidos
		then PROHIBIDO
		else (bind ?h1 (poner-aspiradora(extrae-h1 ?estado)))
			 (bind ?h2 (quitar-aspiradora(extrae-h2 ?estado)))
			 (create$ ?h1 ?h2)
	)
)

;Operador Derecha
(deffunction D ($?estado)
	(bind ?h2 (extrae-h2 ?estado))
	
	(if (and (esta-aspiradora? ?h2) ?*CON-PROHIBIDO*) ;Si ya está la aspiradora en la derecha y están activas los estados prohibidos
		then PROHIBIDO
		else (bind ?h1 (quitar-aspiradora(extrae-h1 ?estado)))
			 (bind ?h2 (poner-aspiradora(extrae-h2 ?estado)))
			 (create$ ?h1 ?h2)
			 
	)
)

(deffunction prohibido? ($?estado)
	(eq $?estado (create$ PROHIBIDO))
)

;Crea la llamada al operador ?operador pasandole el estado $?estado, y después evalúa la llamada 
(deffunction aplicar-operador (?operador $?estado)
	(eval
		(format nil "( %s (create$ %s))" ?operador (implode$ ?estado))
	)
)

; Devuelve la lista de hijos tras aplicar todos los operadores
; Además rellena ?*LISTA-DE-OPERADORES* con los operadores que se han ido aplicando
(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (and (not (prohibido? ?hijo)) (or (not ?*CON-VISITADOS*) (and ?*CON-VISITADOS* (not (member$ (implode$ ?hijo) ?*VISITADOS*))))) 
			then (bind ?lista-hijos (create$ ?lista-hijos (implode$ ?hijo)))
				 (bind ?*LISTA-DE-OPERADORES* (create$ ?*LISTA-DE-OPERADORES* ?op))
				 (if ?*CON-VISITADOS* then (bind ?*VISITADOS* (create$ ?*VISITADOS* (implode$ ?hijo))))
		)
	)
	;si la ultima instruccion a ejecutar es el if sin entrar, devuelve un false y no la lista de hijos
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

;Realiza una busqueda en profundidad devolviendo un arbol en formato para su uso en graphviz
(deffunction busqueda-en-profundidad-arbol ($?lista)
	(printout t "Introduzca el nombre del fichero en el que se escribirá el árbol: ")
	(bind ?file (read))
	(open ?file ?*GRAPH-FILE* "a")
	(printout ?*GRAPH-FILE* "digraph G {" crlf)
	(bind ?padre-str (state-to-str (implode$ ?*PADRE*)))
	(format ?*GRAPH-FILE* "    %s [fillcolor=yellow, style=filled];%n    n0 [label=\"\", shape=none, height=.0, width=.0];%n" ?padre-str)
	(format ?*GRAPH-FILE* "    n0 -> %s;%n" ?padre-str)
	(bind ?i 0)
	(while (and(not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
		(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1  ?*LISTA*)))
		(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
		(if (not (exito ?*PADRE*)) then 
			(bind ?hijos (hijos ?*PADRE*))
			(loop-for-count (?j 1 (length$ ?hijos)) ; Se recorren los hijos para ir imprimiendolos en el fichero
				(bind ?hijo (nth ?j ?hijos))
				(print-edge (state-to-str (implode$ ?*PADRE*)) (state-to-str ?hijo) (create$ (nth ?j ?*LISTA-DE-OPERADORES*)))
				(if (exito (explode$ ?hijo))
					then (format ?*GRAPH-FILE* "    %s [fillcolor=green, style=filled];%n" (state-to-str ?hijo))
				)
			)
			(bind ?*LISTA-DE-OPERADORES* (create$)) ; Se limpia la lista de operadores
			(bind ?*LISTA* (create$ ?hijos ?*LISTA*)))
		(bind ?i (+ ?i 1))
		(if (and (> ?*PASOS* 0) (= ?i ?*PASOS*)) then (break))
	)

	(if  (exito ?*PADRE*) 
		then (printout t "La solución es " ?*PADRE* crlf)
		else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
	)

	(printout ?*GRAPH-FILE* } crlf)
	(close ?*GRAPH-FILE*)
)
