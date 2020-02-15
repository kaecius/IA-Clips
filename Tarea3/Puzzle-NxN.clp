; TODO implement logics

;generate list : (loop-for-count (?x 1 n) (bind ?list (create$ ?list ?x)))

(defglobal
    ?*OPERADORES* = (create$ N S E O)
    ?*SECUENCIA-DE-OPERADORES = (create$)
    ?*LISTA* = (create$)
    ?*PADRE* = (create$)
)

(deffunction exito? ($?estado)
    (eval (format nil "(< %s)" (implode$ ?estado)))
)