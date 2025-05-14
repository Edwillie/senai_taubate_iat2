(deftemplate maquina
    (slot id (type STRING))
    (slot horas-operacao (type INTEGER) (default 0))
)

(defrule manutencao-urgente
    (declare (salience 10))
    (maquina (id ?id) (horas-operacao ?horas&:(>= ?horas 1000)))
    =>
    (printout t "Maquina " ?id ": Manuten‡Æo URGENTE necess ria (" ?horas " horas)." crlf)
)

(defrule manutencao-planejada
    (declare (salience 0))
    (maquina (id ?id) (horas-operacao ?horas&:(>= ?horas 1000)))
    =>
    (printout t "Maquina " ?id ": Agendar manuten‡Æo planejada (" ?horas " horas)." crlf)
)

; (set-strategy depth)
; (assert (maquina (id "M002") (horas-operacao 1500)))

; (set-strategy breadth)
; (assert (maquina (id "M002") (horas-operacao 1500)))