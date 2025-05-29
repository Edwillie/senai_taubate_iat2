;;Como calcular o ¡ndice OEE?
;;Disponibilidade% = (Tempo produzindo / Tempo programado para produzir) * 100%
;;Performance% = (Quantidade Produ‡Æo Real / Quantidade Produ‡Æo Te¢rica) * 100%
;;Qualidade% = (Quantidade de Itens Bons / Quantidade Total Produzida) 100%
;;OEE% = Disponibilidade% * Performance% * Qualidade%
;---------------------------------
; ciclo = prod_minutos / prod_qtde_pecas (tempo por pe‡a)
; disponibilidade = ((plan_minutos - prod_minutos_parado) / plan_minutos) * 100
; performance = (prod_qtde_pecas / plan_qtde_pecas) * 100
; qualidade = (prod_qtde_pecas / prod_qtde_pecas_refugo) * 100     

(deftemplate equipamento
    (slot equipamento_nome (type STRING))
    (slot tempo_ciclo_ideal (type INTEGER) (default 0))
    (slot status (type STRING))
)

(deftemplate producao
    (slot equipamento_nome (type STRING))
    (slot prod_minutos_parado (type INTEGER) (default 0))
    (slot prod_qtde_pecas (type INTEGER) (default 0))
    (slot prod_qtde_pecas_refugo (type INTEGER) (default 0))
)

(deftemplate planejado
    (slot equipamento_nome (type STRING))
    (slot plan_minutos (type INTEGER) (default 0))
    (slot plan_qtde_pecas (type INTEGER) (default 0))
)

(deftemplate performance
    (slot equipamento_nome (type STRING))
    (slot perf_minutos_trabalho (type INTEGER) (default 0))
    (slot perf_rend (type FLOAT) (default 0.0))
    (slot perf_disp (type FLOAT) (default 0.0))
    (slot perf_OEE (type FLOAT) (default 0.0))
)

(deffacts startup
   (fase solicitar-eqp)
)

(defrule coletar_dados
    ?f <- (fase solicitar-eqp)
    => 
    (printout t "------ INFORME O EQUIPAMENTO ------" crlf)
    (printout t "Informe o equipamento (ou digite sair para encerrar o programa): ")
    (bind ?eqp (read))
    (if (or (eq ?eqp sair) (eq ?eqp "sair")) then
        (printout t "Saindo do programa." crlf)
        (assert (fase encerrar-programa))
     else
        (assert (eqp-digitado ?eqp))
        (assert (fase verificar-existencia))   
    )
)

;(defrule atualiza_prd_eqp
;    ?cur_eqp <- (equipamento (equipamento_nome ?eqp))
;    ?lst_eqp <- (producao (equipamento_nome ?eqp))
;    =>
;    (printout t "------ ATUALIZE O EQUIPAMENTO ------" crlf)
;    (printout t "Informe o Tempo parado (min): ")
;    (bind ?tpo_stop (read))
;    (printout t "Informe qtde pe‡as produzidas: ")
;    (bind ?qtde_prod (read))
;    (assert (equipamento    (equipamento_nome ?eqp)
;                            (status "Atualizado!")
;            )
;            (producao       (equipamento_nome ?eqp)
;                            (tempo_minutos_parado ?tpo_stop)
;                            (qtde_pecas ?qtde_prod)
;            )
;    )
;    (retract ?cur_eqp)
;)
;
;(defrule coleta_novo_eqp
;    ?cur_eqp <- (equipamento (equipamento_nome ?eqp))
;    (not (producao (equipamento_nome ?eqp)))
;    =>
;    (printout t "------ NOVO O EQUIPAMENTO ------" crlf)
;    (printout t "Informe o Tempo planejado (min): ")
;    (bind ?tpo_plan (read))
;    (printout t "Informe o Tempo ideal (min): ")
;    (bind ?tpo_ideal (read))
;    (printout t "Informe o Tempo parado (min): ")
;    (bind ?tpo_stop (read))
;    (printout t "Informe qtde pe‡as produzidas: ")
;    (bind ?qtde_prod (read))
;    (assert (equipamento    (equipamento_nome ?eqp)
;                            (tempo_ciclo_ideal ?tpo_ideal)
;                            (status "Novo!")
;            )
;            (producao       (equipamento_nome ?eqp)
;                            (tempo_minutos_planejado ?tpo_plan)
;                            (tempo_minutos_parado ?tpo_stop)
;                            (qtde_pecas ?qtde_prod)
;            )
;    )
;    (retract ?cur_eqp)
;)
;
;(defrule calcular_oee
;)
;
;(defrule monitorar_eficiencia
;)

;(defrule ajuste_automatico
;)

;(defrule redistribuir_tarefa
;)
;
;(defrule apresenta_dados
;    (equipamento
;        (equipamento_nome ?eqp)
;        (tempo_ciclo_ideal ?tpo_ideal)
;        (status ?sts)
;    )
;    (producao
;        (equipamento_nome ?eqp)
;        (tempo_minutos_planejado ?tpo_plan)
;        (tempo_minutos_parado ?tpo_stop)
;        (qtde_pecas ?qtde_prod)
;    )
;    =>
;    (printout t "O equipamento: " ?eqp " (" ?sts ") tem o ciclo ideal de " ?tpo_ideal "(min) com planejamento de " ?tpo_plan "(min) mas ficou parado " ?tpo_stop "(min) produzindo um total de " ?qtde_prod " pecas." crlf)
;)