; TODO implement logics

;generate list : (loop-for-count (?x 1 n) (bind ?list (create$ ?list ?x)))

;(N (create$ 1 2 3 4 5 6 7 8 9))
(defglobal
    ?*OPERADORES* = (create$ N S E O)
    ?*LISTA* = (create$)
    ?*PADRE* = (create$)
    ?*VISITADOS* = (create$)
    ?*CON-VISITADOS* = FALSE
    ?*HUECO* = nil
    ?*DIMENSION* = nil ;sqrt length
)
;F?
;OPT?
;guardar hueco como global?
;guardar raiz n?



(deffunction swap-hueco-ficha (?posicion-hueco ?posicion-ficha $?estado)
    (bind ?ficha (nth ?posicion-ficha ?estado))
    (bind ?estado (replace$ ?estado ?posicion-hueco ?posicion-hueco ?ficha))
    (replace$ ?estado ?posicion-ficha ?posicion-ficha ?*HUECO*)
)

;PROHIBIDO: Ir al norte estando en la primera fila
(deffunction N ($?estado)
    (bind ?posicion-hueco (member$ ?*HUECO* ?estado))
    (if (<= ?posicion-hueco ?*DIMENSION*)
        then PROHIBIDO
        else 
        (bind ?posicion-ficha-norte (- ?posicion-hueco ?*DIMENSION*))
        (bind ?estado (swap-hueco-ficha ?posicion-hueco ?posicion-ficha-norte ?estado))
        (create$ ?estado N)
    )
)

;PROHIBIDO: Ir al sur estando en la ultima fila
(deffunction S ($?estado)
    (bind ?posicion-hueco (member$ ?*HUECO* ?estado))
    (if (>= ?posicion-hueco (* ?*DIMENSION* (- ?*DIMENSION* 1)) ) ;posicion-hueco >= D*(D-1)
        then PROHIBIDO
        else
            (bind ?posicion-ficha-sur (+ ?posicion-hueco ?*DIMENSION*))
            (bind ?estado (swap-hueco-ficha ?posicion-hueco ?posicion-ficha-sur ?estado))
            (create$ ?estado S)
    )
    
)

;PROHIBIDO: Ir al E estando a la derecha
(deffunction E ($?estado)
    (bind ?posicion-hueco (member$ ?*HUECO* ?estado))
    (if (= (mod ?posicion-hueco ?*DIMENSION*) 0)
        then PROHIBIDO
        else
            (bind ?posicion-ficha-este (+ ?posicion-hueco 1))
            (bind ?estado (swap-hueco-ficha ?posicion-hueco ?posicion-ficha-este ?estado))
            (create$ ?estado E)
    )
)

;PROHIBIDO: Ir al O estando a la izquierda
(deffunction O ($?estado)
    (bind ?posicion-hueco (member$ ?*HUECO* ?estado))
    (if (= (mod ?posicion-hueco ?*DIMENSION*) 1)
        then PROHIBIDO
        else
            (bind ?posicion-ficha-oeste (- ?posicion-hueco 1))
            (bind ?estado (swap-hueco-ficha ?posicion-hueco ?posicion-ficha-oeste ?estado))
            (create$ ?estado O)
    )
)


(deffunction h ($?puzzle)
    (bind ?total 0)
    (loop-for-count (?i 1 (* ?*DIMENSION* ?*DIMENSION*))
        (if (not(eq (nth ?i ?puzzle) ?i))
            then (bind ?total (+ ?total 1))
        )
    )
    ?total
)

(deffunction aplicar-operador (?operador $?estado)
	(funcall  ?operador $?estado)
)

(deffunction extrae-cop ($?estado)
	(bind ?posicion-cop (member$ cop ?estado))
	(bind ?ultima (length$ ?estado))
	(subseq$ ?estado  ?posicion-cop ?ultima)
)

(deffunction extrae-estado ($?estado)
	(bind ?posicion-cop (member$ cop ?estado))
	(subseq$ ?estado 0 (- ?posicion-cop 1))
)

(deffunction cantidad-operadores($?estado)
    (- (length$ (extrae-cop ?estado)) 1)
)

(deffunction ordenar-f (?f ?puzzle1 ?puzzle2)
    (> (funcall ?f ?puzzle1) (funcall ?f ?puzzle2))
)

(deffunction prohibido? ($?estado)
  	(eq $?estado (create$ PROHIBIDO))
)

(deffunction exito? ($?estado)
    (eval (format nil "(< %s)" (implode$ (extrae-estado ?estado ))))
)


(deffunction operadores-hijos($?estado)
	(bind $?lista-operadores (create$))
	(progn$ (?op ?*OPERADORES*) 
		(bind $?hijo (aplicar-operador ?op ?estado))
		(if (not (prohibido? ?hijo)) then 
			(bind ?lista-operadores (create$ ?lista-operadores ?op)))
	)
	?lista-operadores
)

(deffunction hijos($?estado)
	(bind $?lista-hijos (create$))
    ;(printout t "bind" crlf)
	(progn$ (?op ?*OPERADORES*)
        ;(printout t "progn" crlf) 
		(bind ?hijo (aplicar-operador ?op ?estado))
        ;(printout t "bind hijo aplicar operador: " ?hijo crlf)
		(if (not (prohibido? ?hijo)) then
            ;(printout t "no prohibido" crlf)
            (if (eq ?*CON-VISITADOS* TRUE)
                then
                    (if (not (member$ ?hijo ?*VISITADOS*))
                        then
                            (bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo)))
                            (bind ?*VISITADOS* (create$ ?*VISITADOS* ?hijo))
                    )
                else
        			(bind ?lista-hijos (create$ ?lista-hijos (implode$  ?hijo)))

            )

		)
	)
	?lista-hijos
)

(deffunction BM (?edoIni ?direccion ?f ?opt $?visitados)
    (bind ?*HUECO* (length$ ?edoIni))
    (bind ?*DIMENSION* (integer (sqrt ?*HUECO*)))
    (bind ?*PADRE* (create$ ?edoIni cop))
    (bind ?*CON-VISITADOS* (first$ ?visitados))
    (bind ?solucion nil)
    (bind ?n-op-solucion nil)
    (bind ?*LISTA* (create$ (implode$ ?*PADRE*)))
    (bind ?i 0)

	(while (not (eq ?*LISTA* (create$))) do
		;(printout t "Paso " ?i crlf)
		(bind ?*PADRE*  (explode$(nth$ 1 ?*LISTA*)))
		;(printout t "Padre " ?*PADRE* crlf)
		(bind ?*LISTA*(rest$ ?*LISTA*))
        ;(printout t "LISTA:" ?*LISTA* crlf)
		(if (not (exito? ?*PADRE*)) 
            then 
                ;(printout t "hijos")
                (bind ?hijos (hijos ?*PADRE*))
                ;(printout t "HIJOS SIN ORDENAR:" ?hijos crlf)
                (if (eq ?direccion anchura)
                    then (bind ?*LISTA* (create$ ?*LISTA* ?hijos))   
                    else (bind ?*LISTA* (create$ ?hijos ?*LISTA*))  
                )
                (bind ?hijos (sort ?f ?hijos))
                ;(printout t "HIJOS ORDENADOS:" ?hijos crlf)
            else
                (printout t "Paso " ?i crlf)
                (printout t "Solución intermedia encontrada" crlf)
                (printout t "Padre " ?*PADRE* crlf)
                (bind ?num-operadores (cantidad-operadores ?*PADRE*))
                (if (eq ?solucion nil)
                    then
                        (bind ?solucion ?*PADRE*)
                        (bind ?n-op-solucion ?num-operadores)
                    else
                        (bind ?opt-resultado (funcall ?opt ?num-operadores ?n-op-solucion))
                        (if (not (= ?opt-resultado ?n-op-solucion))
                            then
                                (bind ?solucion ?*PADRE*)
                                (bind ?n-op-solucion ?num-operadores)
                        )
                )
        )
		(bind ?i (+ ?i 1))
	)
	(if  (eq ?solucion nil ) 
		then (printout t "No hay solución" crlf) 
		else (printout t "La solución óptima es " ?solucion crlf) 
	)

)
;(load Puzzle-NxN.clp)
; (BM (create$ 3 9 4 5 8 7 2 1 6) anchura h min TRUE)
