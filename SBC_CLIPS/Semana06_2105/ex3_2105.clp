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
    (slot plan_minutos (type INTEGER) (default 0))
    (slot tempo_ciclo_ideal (type INTEGER) (default 0))
    (slot status (type STRING))
)

(deftemplate producao
    (slot equipamento_nome (type STRING))
    (slot prod_minutos_parado (type INTEGER) (default 0))
    (slot prod_qtde_pecas (type INTEGER) (default 0))
    (slot prod_qtde_pecas_refugo (type INTEGER) (default 0))
)

(deftemplate performance
    (slot equipamento_nome (type STRING))
    (slot perf_minutos_trabalho (type INTEGER) (default 0))
    (slot perf_rend (type FLOAT) (default 0.0))
    (slot perf_disp (type FLOAT) (default 0.0))
    (slot perf_qual (type FLOAT) (default 0.0))
    (slot perf_OEE (type FLOAT) (default 0.0))
)

(deffacts startup
   (fase solicitar-eqp)
)

(defrule coletar_dados
    ?f <- (fase solicitar-eqp)
    => 
    (printout t  crlf)
    (printout t  crlf)
    (printout t  crlf)
    (printout t "------------------------------------------------------------------------------" crlf)
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
    (retract ?f)
)

(defrule novo-equipamento
    ?f <- (fase verificar-existencia)
    ?v <- (eqp-digitado ?eqp)
    (not (equipamento (equipamento_nome ?eqp)))
    =>
    (printout t  crlf)
    (printout t  crlf)
    (printout t  crlf)
    (printout t "------------------------------------------------------------------------------" crlf)
    (printout t "----- NOVO EQUIPAMENTO: " ?eqp " -----" crlf)
    (printout t "Informe o Tempo de Ciclo Ideal desse equipamento: ")
    (bind ?tci (read))
    (printout t "Informe a quantidade de pe‡as PRODUZIDAS: ")
    (bind ?qpp (read))
    (printout t "Informe a quantidade de pe‡as DEFEITUOSAS: ")
    (bind ?qpd (read))
    (printout t "Informe o tempo de equipamento PLANEJADO DE TRABALHO: ")
    (bind ?tept (read))
    (printout t "Informe o tempo de equipamento PARADO: ")
    (bind ?tes (read))
    (assert (equipamento (equipamento_nome ?eqp)
                         (plan_minutos ?tept)
                         (tempo_ciclo_ideal ?tci))
            (producao (equipamento_nome ?eqp)
                      (prod_minutos_parado ?tes)
                      (prod_qtde_pecas ?qpp)
                      (prod_qtde_pecas_refugo ?qpd))             
    )
    ;(assert (fase solicitar-eqp))
    (assert (fase calculos-processos))
    (retract ?f ?v)
)

(defrule lancar-producao
    ?f <- (fase verificar-existencia)
    ?v <- (eqp-digitado ?eqp)
    (equipamento (equipamento_nome ?eqp))
    =>
    (printout t  crlf)
    (printout t  crlf)
    (printout t  crlf)
    (printout t "------------------------------------------------------------------------------" crlf)
    (printout t "----- ATUALIZA PRODUCAO: " ?eqp " -----" crlf)
    (printout t "Informe a quantidade de pe‡as PRODUZIDAS: ")
    (bind ?qpp (read))
    (printout t "Informe a quantidade de pe‡as DEFEITUOSAS: ")
    (bind ?qpd (read))
    (printout t "Informe o tempo de equipamento PARADO: ")
    (bind ?tes (read))
    (assert (producao (equipamento_nome ?eqp)
                      (prod_minutos_parado ?tes)
                      (prod_qtde_pecas ?qpp)
                      (prod_qtde_pecas_refugo ?qpd))             
    )
    ;(assert (fase solicitar-eqp))
    (assert (fase calculos-processos))
    (retract ?f ?v)
)

(defrule calcular-indicadores
    ?f <- (fase calculos-processos)
    (equipamento (equipamento_nome ?eqp)
                 (plan_minutos ?tept)
                 (tempo_ciclo_ideal ?tci))
    (producao (equipamento_nome ?eqp)
              (prod_minutos_parado ?tes)
              (prod_qtde_pecas ?qpp)
              (prod_qtde_pecas_refugo ?qpd))
    =>
    (printout t  crlf)
    (printout t  crlf)
    (printout t  crlf)
    (printout t "------------------------------------------------------------------------------" crlf)
    (printout t "----- CALCULANDO INDICADORES " ?eqp " -----" crlf)
    ;(- ?tept ?tes) --------------------> Tempo Trabalhado = plan_minutos - prod_minutos_parado
    ;(/ (- ?tept ?tes) ?tept) ----------> Disponibilidade  = Tempo Trabalhado / plan_minutos 
    ;(/ (* ?qpp ?tci) (- ?tept ?tes)) --> Performance      = (Qtde Produzida * Tempo Cilo Ideal) / Tempo Trabalhado 
    ;(/ (- ?qpp ?qpd) ?qpp) ------------> Qualidade        = (Qtde Produzida - Qtde Refugo) / Qtde Produzida
    (assert (performance (equipamento_nome ?eqp)
                         (perf_minutos_trabalho (- ?tept ?tes))
                         (perf_qual (/ (- ?qpp ?qpd) ?qpp))
                         (perf_rend (/ (* ?qpp ?tci) (- ?tept ?tes)))
                         (perf_disp (/ (- ?tept ?tes) ?tept)))            
    )
    (printout t "Disponibilidade = " (/ (- ?tept ?tes) ?tept) crlf)
    (printout t "Performance.... = " (/ (* ?qpp ?tci) (- ?tept ?tes)) crlf)
    (printout t "Qualidade...... = " (/ (- ?qpp ?qpd) ?qpp) crlf)
    (assert (fase solicitar-eqp))
    (retract ?f)
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