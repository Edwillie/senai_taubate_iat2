(deftemplate material
    (slot identificador (type STRING))
    (slot estoque-atual (type INTEGER) (default 0))
    (slot demanda-mensal (type INTEGER) (default 0))
    (slot tempo-entrega (type INTEGER) (default 0))
)

(deftemplate recomendacao
    (slot identificador (type STRING))
    (slot acao (type STRING))
)

(defrule coletar-dados-material
    =>
    (printout t "Digite o identificador do material: ")
    (bind ?id (read))
    (printout t "Digite o estoque atual (unidades): ")
    (bind ?estoque (read))
    (printout t "Digite a demanda mensal (unidades): ")
    (bind ?demanda (read))
    (printout t "Digite o tempo de entrega (dias): ")
    (bind ?entrega (read))
    (assert (material   (identificador ?id) 
                        (estoque-atual ?estoque) 
                        (demanda-mensal ?demanda) 
                        (tempo-entrega ?entrega)
            )
    )
)

(defrule recomendar-reposicao-urgente
    (declare (salience 20))
    (material   (identificador ?id) 
                (estoque-atual ?estoque) 
                (demanda-mensal ?demanda)
    ) 
    (test (< ?estoque (* ?demanda 0.1)))
    =>
    (assert (recomendacao   (identificador ?id) 
                            (acao "reposi‡Æo urgente")
            )
    )
)

(defrule recomendar-reposicao-planejada
    (declare (salience 10))
    (material (identificador ?id) (estoque-atual ?estoque) (demanda-mensal ?demanda) (tempo-entrega ?entrega))
    (test (and (< ?estoque (* ?demanda 0.5)) (> ?entrega 7)))
    =>
    (assert (recomendacao (identificador ?id) (acao "reposi‡Æo planejada")))
)

(defrule recomendar-sem-reposicao
    (declare (salience 5))
    (material (identificador ?id) (estoque-atual ?estoque) (demanda-mensal ?demanda))
    (test (>= ?estoque (* ?demanda 0.5)))
    (not (recomendacao (identificador ?id)))
    =>
    (assert (recomendacao (identificador ?id) (acao "estoque suficiente")))
)

(defrule processar-recomendacao
    (recomendacao (identificador ?id) (acao ?acao))
    =>
    (printout t "Material " ?id ": " ?acao "." crlf)
)