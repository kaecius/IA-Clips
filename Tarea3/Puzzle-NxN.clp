; (load Puzzle-NxN.clp)
; (BM (create$ 3 5 9 6 2 16 1 4 7 8 10 11 12 13 14 15) anchura h-puzzle min TRUE)

(defglobal
    ?*ESTADO-INICIAL* = (create$)
    ?*PADRE* = ?*ESTADO-INICIAL*
    ?*LISTA* = (create$)
    ?*LISTA-F* = (create$)
    ?*LISTA-OPERADORES* = (create$)
    ?*DIMENSION* = 4
    ?*SIMBOLO-HUECO* = 0
    ?*OPERADORES* = (create$ S E N O)
    ?*SECUENCIA-OPERADORES* = (create$)
    ?*VISITADOS* = (create$)
    ?*FUNCION-H* = nil
    ?*CON-PROHIBIDO* = TRUE
    ?*CON-VISITADOS* = TRUE
)

(deffunction modulo (?x $?y)
    (bind ?cociente (/ ?x (nth 1 $?y)))
    (integer (* (- ?cociente (integer ?cociente)) (nth 1 $?y)))
)

(deffunction posicion-hueca ($?estado)
    (member$ ?*SIMBOLO-HUECO* ?estado)
)

(deffunction N ($?estado)
    (bind ?hueco (posicion-hueca ?estado))
    (if (<= ?hueco ?*DIMENSION*)
        then PROHIBIDO
        else (create$ (subseq$ ?estado 1 (- (- ?hueco ?*DIMENSION*) 1))
                      ?*SIMBOLO-HUECO* (subseq$ ?estado (+ (- ?hueco ?*DIMENSION*) 1) (- ?hueco 1))
                      (nth (- ?hueco ?*DIMENSION*) ?estado)
                      (subseq$ ?estado (+ ?hueco 1) (length$ ?estado)))
    )
)

(deffunction S ($?estado)
    (bind ?hueco (posicion-hueca ?estado))
    (if (> ?hueco (* ?*DIMENSION* (- ?*DIMENSION* 1)))
        then PROHIBIDO
        else (create$ (subseq$ ?estado 1 (- ?hueco 1))
                      (nth (+ ?hueco ?*DIMENSION*) ?estado)
                      (subseq$ ?estado (+ ?hueco 1) (- (+ ?hueco ?*DIMENSION*) 1))
                      ?*SIMBOLO-HUECO* (subseq$ ?estado (+ (+ ?hueco ?*DIMENSION*) 1) (length$ ?estado)))
    )
)

(deffunction O ($?estado)
    (bind ?hueco (posicion-hueca ?estado))
    (if (eq (modulo (- ?hueco 1) ?*DIMENSION*) 0)
        then PROHIBIDO
        else (create$ (subseq$ ?estado 1 (- ?hueco 2))
                      ?*SIMBOLO-HUECO* (nth (- ?hueco 1) ?estado)
                      (subseq$ ?estado (+ ?hueco 1) (length$ ?estado)))
    )
)

(deffunction E ($?estado)
    (bind ?hueco (posicion-hueca ?estado))
    (if (eq (modulo ?hueco ?*DIMENSION*) 0)
        then PROHIBIDO
        else (create$ (subseq$ ?estado 1 (- ?hueco 1))
                      (nth (+ ?hueco 1) ?estado) ?*SIMBOLO-HUECO*
                      (subseq$ ?estado (+ ?hueco 2) (length$ ?estado)))
    )
)

(deffunction exito ($?estado)
    (eval (format nil "(< %s)" (implode$ ?estado)))
)

(deffunction aplicar-operador (?operador $?estado)
	(funcall ?operador $?estado)
)

(deffunction prohibido? ($?estado)
  	(eq $?estado (create$ PROHIBIDO))
)

(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind ?hijo (aplicar-operador ?op ?estado))
		(if (and (not (prohibido? ?hijo)) (and ?*CON-VISITADOS* (not (member$ (implode$ ?hijo) ?*VISITADOS*)))) then 
			(bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo)))
            (bind ?*LISTA-F* (create$ ?*LISTA-F* (funcall ?*FUNCION-H* ?hijo)))
            (bind ?*VISITADOS* (create$ ?*VISITADOS* (implode$ ?hijo)))
            (bind ?*LISTA-OPERADORES* (create$ ?*LISTA-OPERADORES* ?op))
		)
	)
	?lista-hijos
)

(deffunction h-puzzle ($?estado)
    (bind ?i 1)
    (bind ?descolocadas 0)
    (progn$ (?x ?estado)
        (if (not (eq ?i ?x))
            then (bind ?descolocadas (+ ?descolocadas 1))
        )
        (bind ?i (+ ?i 1))
    )
    ?descolocadas
)

(deffunction map2D ($?x)
    (bind ?cociente (/ (nth 1 $?x) ?*DIMENSION*))
    (bind ?coordX (integer ?cociente))
    (if (< ?coordX (/ (nth 1 $?x) ?*DIMENSION*))
        then (bind ?coordX (+ ?coordX 1))
             (bind ?coordY (modulo (nth 1 $?x) ?*DIMENSION*))
        else (bind ?coordY ?*DIMENSION*)
    )

    (create$ ?coordX ?coordY)
)

(deffunction manhattan ($?estado)
    (bind ?distance 0)
    (loop-for-count (?i 1 ?*DIMENSION*)
        (bind ?desiredCoords (map2D (nth ?i ?estado)))
        (bind ?currentCoords (map2D ?i))
        (bind ?offset (+ (abs (- (nth 1 ?desiredCoords) (nth 1 ?currentCoords))) (abs (- (nth 2 ?desiredCoords) (nth 2 ?currentCoords)))))
        (bind ?distance (+ ?distance ?offset))
    )
    ?distance
)

(deffunction BM (?edo-inicial ?direccion ?funcion ?optimizador $?visitados)
    (bind ?*ESTADO-INICIAL* ?edo-inicial)
    (bind ?*FUNCION-H* ?funcion)
    (bind ?*SIMBOLO-HUECO* (length$ ?edo-inicial))
    (bind ?*DIMENSION* (integer (sqrt (length$ ?*ESTADO-INICIAL*))))
    (bind ?*LISTA* (create$ (implode$ ?*ESTADO-INICIAL*)))
    (bind ?*LISTA-F* (create$ (funcall ?*FUNCION-H* ?*ESTADO-INICIAL*)))
    (bind ?*PADRE* ?*ESTADO-INICIAL*)
    (bind ?*CON-VISITADOS* (nth 1 $?visitados))
    (bind ?*VISITADOS* (create$ (implode$ ?*ESTADO-INICIAL*)))
    (if (eq (length$ ?*ESTADO-INICIAL*) (* ?*DIMENSION* ?*DIMENSION*))
        then (bind ?i 0)  ; Board dimensions are OK
             (while (and(not (exito ?*PADRE*)) (not (eq ?*LISTA* (create$)))) do
                 (printout t "Paso " ?i crlf)
                 (if (eq (length$ ?*LISTA-F*) 1) ; min and max functions expect at least 2 arguments
                    then (bind ?next-father 1)
                    else (bind ?next-father (eval (format nil "(member$ (%s %s) ?*LISTA-F*)" ?optimizador (implode$ ?*LISTA-F*))))
                 )
                 (bind ?*PADRE* (explode$ (nth ?next-father ?*LISTA*)))
                 (bind ?*SECUENCIA-OPERADORES* (create$ ?*SECUENCIA-OPERADORES* (nth ?next-father ?*LISTA-OPERADORES*)))
                 (printout t "Padre " ?*PADRE* crlf)
                 (bind ?*LISTA* (create$ (subseq$ ?*LISTA* 1 (- ?next-father 1)) (subseq$ ?*LISTA* (+ ?next-father 1) (length$ ?*LISTA*))))
                 (bind ?*LISTA-F* (create$ (subseq$ ?*LISTA-F* 1 (- ?next-father 1)) (subseq$ ?*LISTA-F* (+ ?next-father 1) (length$ ?*LISTA-F*))))
                 (bind ?*LISTA-OPERADORES* (create$ (subseq$ ?*LISTA-OPERADORES* 1 (- ?next-father 1)) (subseq$ ?*LISTA-OPERADORES* (+ ?next-father 1) (length$ ?*LISTA-OPERADORES*))))
                 (if (not (exito ?*PADRE*)) 
                    then (bind ?hijos (hijos ?*PADRE*))
                         (if (eq ?direccion profundidad)
                             then (bind ?*LISTA* (create$ ?hijos ?*LISTA*))
                             else (bind ?*LISTA* (create$ ?*LISTA* ?hijos))
                         )
                    else (break)
                 )
                 (bind ?i (+ ?i 1))
             )
 
             (if  (exito ?*PADRE*) 
                 then (printout t "La solución es: " ?*SECUENCIA-OPERADORES* crlf)
                 else (if (=(length$ ?*LISTA*)0)  then (printout t "No hay solución" crlf))
             )
        else (printout t "Dimensión del tablero incorrecta." crlf)
    )
)