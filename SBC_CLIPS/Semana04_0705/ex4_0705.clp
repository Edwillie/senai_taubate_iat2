(deftemplate maquina
    (slot id (type STRING))
    (slot horas-operacao (type INTEGER) (default 0))
)

(deftemplate recomendacao
    (slot id (type STRING))
    (slot acao (type STRING))
)

(defrule coletar-dados-maquina
    =>
    (printout t "Digite o ID da m quina: ")
    (bind ?id (read))
    (printout t "Digite as horas de operacao: ")
    (bind ?horas (read))
    (assert (maquina (id ?id) (horas-operacao ?horas)))
)

(defrule recomendar-manutencao
    (maquina (id ?id) (horas-operacao ?horas))
    =>
    (if (>= ?horas 1000) then
        (assert (recomendacao (id ?id) (acao "manuten‡Æo preventiva")))
     else
        (assert (recomendacao (id ?id) (acao "continuar operacao")))
    )
)

(defrule processar-recomendacao
    (recomendacao (id ?id) (acao ?acao))
    =>
    (printout t "Recomenda‡Æo para a m quina " ?id ": " ?acao "." crlf)
)