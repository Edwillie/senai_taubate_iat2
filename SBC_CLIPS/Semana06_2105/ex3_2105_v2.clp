;; Definiá∆o do template para os fatos de equipamento
(deftemplate equipamento
   (slot id (type INTEGER))
   (slot nome (type STRING))
   (slot tempo-planejado (type FLOAT))
   (slot eficiencia (type FLOAT))
)

;; Fato inicial para comeáar o fluxo
(deffacts startup
   (fase solicitar-id)
)

;; Regra para solicitar o ID do equipamento ao usu†rio
(defrule solicitar-id-equipamento
   ?f <- (fase solicitar-id)
   =>
   (printout t crlf "----------------------------------------" crlf)
   (printout t "Digite o ID do equipamento (n£mero inteiro, 0 para sair): ")
   (bind ?id (read))
   (if (integerp ?id) then
      (if (eq ?id 0) then
         (printout t "Saindo do programa." crlf)
         (halt)
       else
         (assert (id-digitado ?id))
         (assert (fase verificar-existencia))
      )
    else
      (printout t "ID inv†lido. Por favor, digite um n£mero inteiro." crlf)
      (assert (fase solicitar-id)) ; Volta a pedir o ID
   )
   (retract ?f) ; Retrai o fato (fase solicitar-id)
) 

;; Regra para verificar se o equipamento j† existe
(defrule verificar-equipamento-existente
   (declare (salience 10)) ; Maior prioridade para verificar antes de assumir que n∆o existe
   ?f1 <- (fase verificar-existencia)
   ?f2 <- (id-digitado ?id)
   ?eq <- (equipamento (id ?id) (nome ?nome))
   =>
   (printout t "Equipamento ID " ?id " (" ?nome ") j† cadastrado." crlf)
   (printout t "Iniciando atualizaá∆o de dados..." crlf)
   (assert (atualizar-equipamento ?eq)) ; Passa a referància do fato
   (assert (fase solicitar-dados-atualizacao))
   (retract ?f1 ?f2)
)

;; Regra para lidar com equipamento n∆o existente
(defrule verificar-equipamento-nao-existente
   (declare (salience 0)) ; Menor prioridade
   ?f1 <- (fase verificar-existencia)
   ?f2 <- (id-digitado ?id)
   (not (equipamento (id ?id)))
   =>
   (printout t "Equipamento ID " ?id " n∆o encontrado." crlf)
   (printout t "Iniciando cadastro de novo equipamento..." crlf)
   (assert (cadastrar-equipamento ?id))
   (assert (fase solicitar-dados-cadastro))
   (retract ?f1 ?f2)
)

;; Regra para solicitar dados para um novo equipamento
(defrule solicitar-dados-novo-equipamento
   ?f1 <- (fase solicitar-dados-cadastro)
   ?f2 <- (cadastrar-equipamento ?id)
   =>
   (printout t "Digite o nome do equipamento: ")
   (bind ?nome (readline))
   (printout t "Digite o tempo planejado (horas, ex: 8.0): ")
   (bind ?tempo (read))
   (printout t "Digite a porcentagem de eficiància (ex: 95.5): ")
   (bind ?eficiencia (read))
   (if (and (stringp ?nome) (numberp ?tempo) (numberp ?eficiencia)) then
      (assert (dados-cadastro ?id ?nome ?tempo ?eficiencia))
      (assert (fase realizar-cadastro))
    else
      (printout t "Dados inv†lidos. Cadastro cancelado. Tente novamente." crlf)
      (assert (fase solicitar-id))
   )
   (retract ?f1 ?f2)
)

;; Regra para efetivar o cadastro do novo equipamento
(defrule realizar-cadastro-equipamento
   ?f1 <- (fase realizar-cadastro)
   ?f2 <- (dados-cadastro ?id ?nome ?tempo ?eficiencia)
   =>
   (assert (equipamento (id ?id) (nome ?nome) (tempo-planejado (float ?tempo)) (eficiencia (float ?eficiencia))))
   (printout t "Equipamento ID " ?id " (" ?nome ") cadastrado com sucesso." crlf)
   (assert (fase solicitar-id)) ; Volta para solicitar o pr¢ximo ID
   (retract ?f1 ?f2)
)

;; Regra para solicitar dados para atualizaá∆o de equipamento existente
(defrule solicitar-dados-atualizacao-equipamento
   ?f1 <- (fase solicitar-dados-atualizacao)
   ?f2 <- (atualizar-equipamento ?eq) ; ?eq Ç a referància ao fato equipamento
   =>
   (printout t "Digite o novo tempo planejado (horas, ex: 8.0): ")
   (bind ?tempo (read))
   (printout t "Digite a nova porcentagem de eficiància (ex: 95.5): ")
   (bind ?eficiencia (read))
   (if (and (numberp ?tempo) (numberp ?eficiencia)) then
      (assert (dados-atualizacao ?eq ?tempo ?eficiencia)) ; Passa a referància do fato
      (assert (fase realizar-atualizacao))
    else
      (printout t "Dados inv†lidos. Atualizaá∆o cancelada. Tente novamente." crlf)
      (assert (fase solicitar-id))
   )
   (retract ?f1 ?f2)
)

;; Regra para efetivar a atualizaá∆o do equipamento
(defrule realizar-atualizacao-equipamento
   ?f1 <- (fase realizar-atualizacao)
   ?f2 <- (dados-atualizacao ?eq ?tempo ?eficiencia) ; ?eq Ç a referància ao fato
   =>
   (modify ?eq (tempo-planejado (float ?tempo)) (eficiencia (float ?eficiencia)))
   (bind ?id (fact-slot-value ?eq id)) ; Obtem o ID do fato para a mensagem
   (printout t "Equipamento ID " ?id " atualizado com sucesso." crlf)
   (assert (fase solicitar-id)) ; Volta para solicitar o pr¢ximo ID
   (retract ?f1 ?f2)
)

;; Regra para limpar fatos tempor†rios caso algo dà errado (opcional, mas bom ter)
(defrule cleanup-error-id
   (declare (salience -10))
   ?f <- (id-digitado ?id)
   =>
   (printout t "Erro no fluxo para ID " ?id ". Reiniciando." crlf)
   (retract ?f)
   (assert (fase solicitar-id))
)

(defrule cleanup-error-cadastro-flag
   (declare (salience -10))
   ?f <- (cadastrar-equipamento ?id)
   =>
   (printout t "Erro no fluxo de cadastro para ID " ?id ". Reiniciando." crlf)
   (retract ?f)
   (assert (fase solicitar-id))
)

(defrule cleanup-error-atualizacao-flag
   (declare (salience -10))
   ?f <- (atualizar-equipamento ?eq)
   =>
   (printout t "Erro no fluxo de atualizaá∆o. Reiniciando." crlf)
   (retract ?f)
   (assert (fase solicitar-id))
)


