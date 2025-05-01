(deffacts lista-inicial
    (item leite 2)
    (item pao 1)
    (item ovos 12)
)

(defrule adicionar-item
    =>
    (assert (item manteiga 1))
    (printout t "Item manteiga adicionado com quantidade 1" crlf)
)