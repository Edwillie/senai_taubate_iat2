(deftemplate equipamento
    (slot equipamento_nome (type STRING))
    (slot tempo_ciclo_ideal (type INTEGER) (default 0))
    (slot status (type STRING))
)

(deftemplate producao
    (slot equipamento_nome (type STRING))
    (slot tempo_minutos_planejado (type INTEGER) (default 0))
    (slot tempo_minutos_parado (type INTEGER) (default 0))
    (slot qtde_pecas (type INTEGER) (default 0))
)

(deftemplate performance
    (slot equipamento_nome (type STRING))
    (slot tempo_minutos_trabalho (type INTEGER) (default 0))
    (slot rendimento (type FLOAT) (default 0.0))
    (slot disponibilidade (type FLOAT) (default 0.0))
    (slot OEE (type FLOAT) (default 0.0))
)

(defrule coletar_dados
    => 
    (printout t "------ INFORME O EQUIPAMENTO ------" crlf)
    (printout t "Informe o equipamento (ou digite sair para encerrar o programa): ")
    (bind ?eqp (read))
    (if (eq ?eqp "sair") then
        (printout t "Programa Finalizado!")
        (halt)
     else
        (assert (equipamento (equipamento_nome ?eqp)))   
    )
)

(defrule atualiza_prd_eqp
    ?cur_eqp <- (equipamento (equipamento_nome ?eqp))
    ?lst_eqp <- (producao (equipamento_nome ?eqp))
    =>
    (printout t "------ ATUALIZE O EQUIPAMENTO ------" crlf)
    (printout t "Informe o Tempo parado (min): ")
    (bind ?tpo_stop (read))
    (printout t "Informe qtde pe‡as produzidas: ")
    (bind ?qtde_prod (read))
    (assert (equipamento    (equipamento_nome ?eqp)
                            (status "Atualizado!")
            )
            (producao       (equipamento_nome ?eqp)
                            (tempo_minutos_parado ?tpo_stop)
                            (qtde_pecas ?qtde_prod)
            )
    )
    (retract ?cur_eqp)
)

(defrule coleta_novo_eqp
    ?cur_eqp <- (equipamento (equipamento_nome ?eqp))
    (not (producao (equipamento_nome ?eqp)))
    =>
    (printout t "------ NOVO O EQUIPAMENTO ------" crlf)
    (printout t "Informe o Tempo planejado (min): ")
    (bind ?tpo_plan (read))
    (printout t "Informe o Tempo ideal (min): ")
    (bind ?tpo_ideal (read))
    (printout t "Informe o Tempo parado (min): ")
    (bind ?tpo_stop (read))
    (printout t "Informe qtde pe‡as produzidas: ")
    (bind ?qtde_prod (read))
    (assert (equipamento    (equipamento_nome ?eqp)
                            (tempo_ciclo_ideal ?tpo_ideal)
                            (status "Novo!")
            )
            (producao       (equipamento_nome ?eqp)
                            (tempo_minutos_planejado ?tpo_plan)
                            (tempo_minutos_parado ?tpo_stop)
                            (qtde_pecas ?qtde_prod)
            )
    )
    (retract ?cur_eqp)
)

;(defrule calcular_oee
;)

;(defrule monitorar_eficiencia
;)

;(defrule ajuste_automatico
;)

;(defrule redistribuir_tarefa
;)

(defrule apresenta_dados
    (equipamento
        (equipamento_nome ?eqp)
        (tempo_ciclo_ideal ?tpo_ideal)
        (status ?sts)
    )
    (producao
        (equipamento_nome ?eqp)
        (tempo_minutos_planejado ?tpo_plan)
        (tempo_minutos_parado ?tpo_stop)
        (qtde_pecas ?qtde_prod)
    )
    =>
    (printout t "O equipamento: " ?eqp " (" ?sts ") tem o ciclo ideal de " ?tpo_ideal "(min) com planejamento de " ?tpo_plan "(min) mas ficou parado " ?tpo_stop "(min) produzindo um total de " ?qtde_prod " pecas." crlf)
)