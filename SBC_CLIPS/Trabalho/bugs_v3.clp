;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sistema B†sico de Verificaá∆o e Cadastro de Componentes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------------
;; Definiá∆o dos Templates
;; ------------------------------------------------------------------

(deftemplate COMPONENTE
  "Representa um produto, serviáo ou recurso da empresa."
  (slot nome (type STRING))
  (slot data-implantacao (type STRING) (default ""))
  (slot status (type SYMBOL) (default EmOperacao))
)

(deftemplate controle
  "Gerencia as fases de execuá∆o do sistema."
  (slot fase (type SYMBOL))
)

;; ------------------------------------------------------------------
;; Regras de Inicializaá∆o e Verificaá∆o de Arquivo
;; ------------------------------------------------------------------

(defrule iniciar-sistema
  "Regra que d† in°cio ao processo."
  (declare (salience 100))
  =>
  (printout t "=================================================" crlf)
  (printout t "  Sistema de Cadastro de Componentes" crlf)
  (printout t "=================================================" crlf crlf)
  
  ; Dados de exemplo para simular um componente j† existente na base
  (assert (COMPONENTE (nome "Sistema A") (data-implantacao "10/01/2023") (status EmOperacao)))
  
  (assert (controle (fase verificar-arquivo)))
)

(defrule verificar-existencia-arquivo
  "Verifica se o arquivo COMPONENTES.CSV existe."
  (declare (salience 90))
  ?f <- (controle (fase verificar-arquivo))
  =>
  (retract ?f)
  (printout t "Verificando a existància do arquivo 'COMPONENTES.CSV'..." crlf)
  (if (open "COMPONENTES.CSV" "r")
  then
    (printout t "Arquivo encontrado." crlf crlf)
    (assert (controle (fase carregar-arquivo)))
    (close)
  else
    (printout t "Arquivo N«O encontrado." crlf crlf)
    (assert (controle (fase iniciar-lancamento-manual)))
  )
)

;; ------------------------------------------------------------------
;; Fluxo Autom†tico (se o arquivo existir)
;; ------------------------------------------------------------------

(defrule processar-arquivo-existente
  "Regra para o caso do arquivo ser encontrado."
  (declare (salience 80))
  ?f <- (controle (fase carregar-arquivo))
  =>
  (retract ?f)
  (printout t "L¢gica para carregar e processar o arquivo CSV seria executada aqui." crlf)
  (printout t "Sistema finalizado." crlf)
)


;; ------------------------------------------------------------------
;; Fluxo Manual (se o arquivo N«O existir)
;; ------------------------------------------------------------------

(defrule iniciar-lancamento-manual
  "Informa o usu†rio que o modo manual ser† iniciado."
  (declare (salience 80))
  ?f <- (controle (fase iniciar-lancamento-manual))
  =>
  (retract ?f)
  (printout t "--- TELA DE LANÄAMENTO MANUAL ---" crlf)
  (assert (controle (fase solicitar-nome)))
)

(defrule solicitar-nome-do-componente
  "Solicita o nome do componente ao usu†rio."
  (declare (salience 70))
  ?f <- (controle (fase solicitar-nome))
  =>
  (retract ?f)
  (printout t "Digite o nome do componente: ")
  (bind ?nome (readline))
  ; Verifica se o componente com o nome digitado j† existe
  (if (find-fact ((?comp COMPONENTE)) (eq ?comp:nome ?nome))
  then
    (assert (controle (fase componente-existente) (componente-atual ?nome)))
  else
    (assert (controle (fase componente-nao-existente) (componente-atual ?nome)))
  )
)

(defrule tratar-componente-existente
  "Se o componente j† existe, solicita apenas o novo status."
  (declare (salience 60))
  ?f <- (controle (fase componente-existente) (componente-atual ?nome))
  ?comp <- (COMPONENTE (nome ?nome))
  =>
  (retract ?f)
  (printout t "Componente '" ?nome "' j† existe na base de conhecimento." crlf)
  (printout t "Digite o novo status (Novo, EmOperacao, Obsoleto, Problematico): ")
  (bind ?status (sym-cat (readline)))
  (modify ?comp (status ?status))
  (printout t "Status do componente '" ?nome "' atualizado para: " ?status crlf)
  (facts)
)

(defrule tratar-componente-nao-existente
  "Se o componente n∆o existe, solicita todos os dados para cadastro."
  (declare (salience 60))
  ?f <- (controle (fase componente-nao-existente) (componente-atual ?nome))
  =>
  (retract ?f)
  (printout t "Componente '" ?nome "' n∆o existe. Realizando novo cadastro..." crlf)
  (printout t "Digite a data de implantaá∆o (DD/MM/AAAA): ")
  (bind ?data (readline))
  (printout t "Digite o status (Novo, EmOperacao, Obsoleto, Problematico): ")
  (bind ?status (sym-cat (readline)))
  (assert (COMPONENTE (nome ?nome) (data-implantacao ?data) (status ?status)))
  (printout t "Componente '" ?nome "' cadastrado com sucesso." crlf)
  (facts)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMO USAR:
; 1. Salve este c¢digo como um arquivo .clp
; 2. Para testar o fluxo MANUAL, apenas carregue e execute:
;    (reset)
;    (run)
; 3. Para testar o fluxo AUTOMµTICO, crie um arquivo vazio chamado
;    "COMPONENTES.CSV" na mesma pasta antes de executar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
