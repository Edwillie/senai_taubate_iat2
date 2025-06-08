(deftemplate COMPONENTE
   (slot nome (type STRING))
   (slot data-implantacao (type STRING))
   (slot status (type STRING) (default "Em Operacao"))
)

(deftemplate CATALOGO
   (slot componente (type STRING))
   (slot impacto-padrao (type STRING))
   (slot sla-prioridade-alta (type STRING))
   (slot sla-prioridade-media (type STRING))
   (slot sla-prioridade-baixa (type STRING))
)

(deftemplate BUG
   (slot id-bug (type INTEGER))
   (slot componente (type STRING))
   (slot assunto (type STRING))
   (slot data-abertura (type STRING))
   (slot severidade (type STRING))
   (slot impacto (type STRING))
   (slot urgencia (type STRING))
   (slot prioridade (type STRING))
)


;; Regras de Entrada de Dados
(defrule verificar-arquivos-dados
   (declare (salience 100))
   (not (arquivo-componentes-existe))
   (not (arquivo-catalogo-existe))
   (not (arquivo-bugs-existe))
   =>
   (printout t "Nenhum arquivo de dados encontrado. Operacao manual sera necessaria." crlf)
   (assert (operacao-manual))
)

(defrule verificar-arquivo-componentes-existe
   (declare (salience 90))
   (test (file-exist "componentes.txt"))
   =>
   (assert (arquivo-componentes-existe))
)

(defrule verificar-arquivo-catalogo-existe
   (declare (salience 90))
   (test (file-exist "catalogo.txt"))
   =>
   (assert (arquivo-catalogo-existe))
)

(defrule verificar-arquivo-bugs-existe
   (declare (salience 90))
   (test (file-exist "bugs.txt"))
   =>
   (assert (arquivo-bugs-existe))
)

(defrule apenas-componentes-existe
   (declare (salience 80))
   (arquivo-componentes-existe)
   (not (arquivo-catalogo-existe))
   (not (arquivo-bugs-existe))
   =>
   (printout t "Apenas o arquivo de Componentes encontrado. Operacao manual sera necessaria." crlf)
   (assert (operacao-manual))
)

(defrule erro-apenas-catalogo-ou-bugs
   (declare (salience 80))
   (or (and (arquivo-catalogo-existe) (not (arquivo-componentes-existe)) (not (arquivo-bugs-existe)))
       (and (arquivo-bugs-existe) (not (arquivo-componentes-existe)) (not (arquivo-catalogo-existe))))
   =>
   (printout t "Erro: Falta de dados basicos (Componentes). Encerrando." crlf)
   (halt)
)

(defrule arquivos-componente-e-catalogo-existem
   (declare (salience 80))
   (arquivo-componentes-existe)
   (arquivo-catalogo-existe)
   (not (arquivo-bugs-existe))
   =>
   (printout t "Arquivos de Componente e Catalogo encontrados. Operacao manual sera necessaria." crlf)
   (assert (operacao-manual))
)

(defrule erro-arquivos-componente-e-bugs
   (declare (salience 80))
   (arquivo-componentes-existe)
   (arquivo-bugs-existe)
   (not (arquivo-catalogo-existe))
   =>
   (printout t "Erro: Falta de dados basicos (Catalogo). Encerrando." crlf)
   (halt)
)

(defrule erro-arquivos-catalogo-e-bugs
   (declare (salience 80))
   (arquivo-catalogo-existe)
   (arquivo-bugs-existe)
   (not (arquivo-componentes-existe))
   =>
   (printout t "Erro: Falta de dados basicos (Componentes). Encerrando." crlf)
   (halt)
)

(defrule todos-arquivos-existem
   (declare (salience 80))
   (arquivo-componentes-existe)
   (arquivo-catalogo-existe)
   (arquivo-bugs-existe)
   =>
   (printout t "Todos os arquivos de dados encontrados. Operacao automatica sera iniciada." crlf)
   (assert (operacao-automatica))
)




;; Regras de Classificacao de Componentes
(defrule classificar-componente-novo
   (declare (salience 70))
   ?c <- (COMPONENTE (nome ?nome) (data-implantacao ?data))
   (test (<= (days-since-epoch ?data) 30))
   =>
   (modify ?c (status "Novo"))
   (printout t "Componente " ?nome " classificado como Novo." crlf)
)

(defrule classificar-componente-obsoleto
   (declare (salience 60))
   ?c <- (COMPONENTE (nome ?nome) (data-implantacao ?data))
   (test (> (days-since-epoch ?data) 365))
   =>
   (modify ?c (status "Obsoleto"))
   (printout t "Componente " ?nome " classificado como Obsoleto." crlf)
)

(defrule classificar-componente-problematico
   (declare (salience 50))
   ?c <- (COMPONENTE (nome ?nome) (status "Em Operacao"))
   (not (COMPONENTE (nome ?nome) (status "Novo")))
   (not (COMPONENTE (nome ?nome) (status "Obsoleto")))
   (count-facts ((?b BUG (componente ?nome)))) (> ?b 3)
   =>
   (modify ?c (status "Problematico"))
   (printout t "Componente " ?nome " classificado como Problematico." crlf)
)

(defrule classificar-componente-em-operacao
   (declare (salience 40))
   ?c <- (COMPONENTE (nome ?nome) (status "Em Operacao"))
   (not (COMPONENTE (nome ?nome) (status "Novo")))
   (not (COMPONENTE (nome ?nome) (status "Obsoleto")))
   (not (COMPONENTE (nome ?nome) (status "Problematico")))
   =>
   (printout t "Componente " ?nome " classificado como Em Operacao." crlf)
)

;; Helper function to calculate days since epoch (for demonstration purposes, a real implementation would be more robust)
(deffunction days-since-epoch (?date-str)
  (bind ?day (nth-lex 1 (str-explode ?date-str "/")))
  (bind ?month (nth-lex 2 (str-explode ?date-str "/")))
  (bind ?year (nth-lex 3 (str-explode ?date-str "/")))
  ;; This is a simplified calculation and might not be accurate for all dates
  ;; For a real system, a more robust date calculation library would be needed
  (return (+ (* ?year 365) (* ?month 30) ?day))
)




;; Regras de Classificacao de Bugs
(defrule bug-impacto-vazio
   (declare (salience 100))
   ?b <- (BUG (componente ?comp) (impacto NIL))
   (CATALOGO (componente ?comp) (impacto-padrao ?imp-padrao))
   =>
   (modify ?b (impacto ?imp-padrao))
   (printout t "Impacto do bug " (bug-id ?b) " atualizado para o impacto padrao do componente." crlf)
)

(defrule classificar-bug-altissima-prioridade-1
   (declare (salience 90))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Alta") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Altissima") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Altissima Prioridade." crlf)
)

(defrule classificar-bug-altissima-prioridade-2
   (declare (salience 90))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Media") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Altissima") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Altissima Prioridade." crlf)
)

(defrule classificar-bug-alta-prioridade-1
   (declare (salience 80))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Baixa") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Alta") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Alta Prioridade." crlf)
)

(defrule classificar-bug-alta-prioridade-2
   (declare (salience 80))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Alta") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Alta") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Alta Prioridade." crlf)
)

(defrule classificar-bug-alta-prioridade-3
   (declare (salience 80))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Media") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Alta") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Alta Prioridade." crlf)
)

(defrule classificar-bug-media-alta-prioridade-1
   (declare (salience 70))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Baixa") (impacto "Ampla" | "Externa"))
   =>
   (modify ?b (prioridade "Media_Alta") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Alta Prioridade." crlf)
)

(defrule classificar-bug-alta-prioridade-4
   (declare (salience 80))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Alta") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Alta") (sla "SLA Prioridade Alta"))
   (printout t "Bug " (bug-id ?b) " classificado como Alta Prioridade." crlf)
)

(defrule classificar-bug-media-alta-prioridade-2
   (declare (salience 70))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Media") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Media_Alta") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Alta Prioridade." crlf)
)

(defrule classificar-bug-media-baixa-prioridade-1
   (declare (salience 60))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Baixa") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Media_Baixa") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Baixa Prioridade." crlf)
)

(defrule classificar-bug-media-alta-prioridade-3
   (declare (salience 70))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Alta") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Media_Alta") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Alta Prioridade." crlf)
)

(defrule classificar-bug-media-baixa-prioridade-2
   (declare (salience 60))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Media") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Media_Baixa") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Baixa Prioridade." crlf)
)

(defrule classificar-bug-baixa-prioridade-1
   (declare (salience 50))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Baixa") (impacto "Localizada"))
   =>
   (modify ?b (prioridade "Baixa") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixa Prioridade." crlf)
)

(defrule classificar-bug-media-baixa-prioridade-3
   (declare (salience 60))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Alta") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Media_Baixa") (sla "SLA Prioridade Media"))
   (printout t "Bug " (bug-id ?b) " classificado como Media_Baixa Prioridade." crlf)
)

(defrule classificar-bug-baixa-prioridade-2
   (declare (salience 50))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Media") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Baixa") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixa Prioridade." crlf)
)

(defrule classificar-bug-baixa-prioridade-3
   (declare (salience 50))
   ?b <- (BUG (severidade "Alta Criticidade") (urgencia "Baixa") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Baixa") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixa Prioridade." crlf)
)

(defrule classificar-bug-baixa-prioridade-4
   (declare (salience 50))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Alta") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Baixa") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixa Prioridade." crlf)
)

(defrule classificar-bug-baixissima-prioridade-1
   (declare (salience 40))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Media") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Baixissima") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixissima Prioridade." crlf)
)

(defrule classificar-bug-baixissima-prioridade-2
   (declare (salience 40))
   ?b <- (BUG (severidade "Baixa Criticidade") (urgencia "Baixa") (impacto "Individual"))
   =>
   (modify ?b (prioridade "Baixissima") (sla "SLA Prioridade Baixa"))
   (printout t "Bug " (bug-id ?b) " classificado como Baixissima Prioridade." crlf)
)

;; Helper function to get bug ID (for demonstration purposes)
(deffunction bug-id (?bug-fact)
  (return (send ?bug-fact get-id-bug))
)




;; Regras de Processamento Automatico
(defrule processamento-automatico
   (declare (salience 30))
   (operacao-automatica)
   =>
   (printout t "Iniciando processamento automatico..." crlf)
   ;; Simular leitura de arquivos e assert de fatos
   ;; Em um ambiente real, isso leria de arquivos CSV/TXT
   (assert (COMPONENTE (nome "Sistema A") (data-implantacao "01/01/2025")))
   (assert (COMPONENTE (nome "Sistema B") (data-implantacao "01/01/2024")))
   (assert (COMPONENTE (nome "Sistema C") (data-implantacao "01/01/2023")))
   (assert (CATALOGO (componente "Sistema A") (impacto-padrao "Ampla") (sla-prioridade-alta "4h") (sla-prioridade-media "8h") (sla-prioridade-baixa "24h")))
   (assert (BUG (id-bug 1) (componente "Sistema A") (assunto "Falha de login") (data-abertura "05/06/2025") (severidade "Alta Criticidade") (urgencia "Alta") (impacto NIL)))
   (assert (BUG (id-bug 2) (componente "Sistema B") (assunto "Erro de relatorio") (data-abertura "01/06/2025") (severidade "Baixa Criticidade") (urgencia "Media") (impacto "Individual")))
   (assert (BUG (id-bug 3) (componente "Sistema A") (assunto "Lentidao no sistema") (data-abertura "06/06/2025") (severidade "Alta Criticidade") (urgencia "Media") (impacto "Localizada")))
   (assert (BUG (id-bug 4) (componente "Sistema A") (assunto "Bug de interface") (data-abertura "07/06/2025") (severidade "Baixa Criticidade") (urgencia "Baixa") (impacto "Individual")))
   (assert (BUG (id-bug 5) (componente "Sistema A") (assunto "Bug de calculo") (data-abertura "08/06/2025") (severidade "Alta Criticidade") (urgencia "Alta") (impacto "Ampla")))


   ;; Simular escrita no processamento.log e atualizacao de arquivos
   (open "processamento.log" "a" log-file)
   (printout log-file "\n--- Processamento Automatico ---\n")
   (printout log-file "Componentes Processados:\n")
   (do-for-all-facts ((?c COMPONENTE)) TRUE
      (printout log-file "  Componente: " ?c:nome ", Status: " ?c:status crlf)
   )
   (printout log-file "Bugs Processados:\n")
   (do-for-all-facts ((?b BUG)) TRUE
      (printout log-file "  Bug ID: " ?b:id-bug ", Prioridade: " ?b:prioridade ", SLA: " ?b:sla crlf)
   )
   (close log-file)

   (printout t "Processamento automatico concluido. Verifique processamento.log" crlf)
)




;; Regras de Processamento Manual
(defrule iniciar-operacao-manual
   (declare (salience 20))
   (operacao-manual)
   =>
   (printout t "Iniciando operacao manual..." crlf)
   (assert (solicitar-componente))
)

(defrule solicitar-componente-manual
   (declare (salience 10))
   (solicitar-componente)
   =>
   (retract (solicitar-componente))
   (printout t "Digite o nome do Componente com problema: ")
   (bind ?comp-nome (readline))
   (assert (componente-digitado ?comp-nome))
)

(defrule componente-nao-registrado
   (declare (salience 5))
   ?c <- (componente-digitado ?comp-nome)
   (not (COMPONENTE (nome ?comp-nome)))
   =>
   (retract ?c)
   (printout t "Componente " ?comp-nome " nao registrado. Por favor, forneca os detalhes." crlf)
   (printout t "Data de Implanta‡Æo (DD/MM/AAAA): ")
   (bind ?data-imp (readline))
   (printout t "Impacto para o negocio em caso de parada (Individual, Localizada, Ampla, Externa): ")
   (bind ?impacto-negocio (readline))
   (printout t "SLA Prioridade Alta: ")
   (bind ?sla-alta (readline))
   (printout t "SLA Prioridade Media: ")
   (bind ?sla-media (readline))
   (printout t "SLA Prioridade Baixa: ")
   (bind ?sla-baixa (readline))
   (assert (COMPONENTE (nome ?comp-nome) (data-implantacao ?data-imp)))
   (assert (CATALOGO (componente ?comp-nome) (impacto-padrao ?impacto-negocio) (sla-prioridade-alta ?sla-alta) (sla-prioridade-media ?sla-media) (sla-prioridade-baixa ?sla-baixa)))
   (assert (solicitar-bug ?comp-nome))
)

(defrule componente-registrado
   (declare (salience 5))
   ?c <- (componente-digitado ?comp-nome)
   (COMPONENTE (nome ?comp-nome))
   =>
   (retract ?c)
   (printout t "Componente " ?comp-nome " ja registrado." crlf)
   (assert (solicitar-bug ?comp-nome))
)

(defrule solicitar-bug-manual
   (declare (salience 10))
   ?s <- (solicitar-bug ?comp-nome)
   =>
   (retract ?s)
   (printout t "\n--- Informacoes do Bug ---" crlf)
   (printout t "Assunto do Problema: ")
   (bind ?assunto (readline))
   (printout t "Data de Abertura (DD/MM/AAAA): ")
   (bind ?data-abertura (readline))
   (printout t "Severidade (Alta Criticidade ou Baixa Criticidade): ")
   (bind ?severidade (readline))
   (printout t "Urgencia (Alta, Media ou Baixa): ")
   (bind ?urgencia (readline))
   (printout t "Impacto (Individual, Localizada, Ampla ou Externa - deixe vazio para usar o padrao): ")
   (bind ?impacto (readline))
   (if (eq ?impacto "") then (bind ?impacto NIL))
   (assert (BUG (id-bug (gensym)) (componente ?comp-nome) (assunto ?assunto) (data-abertura ?data-abertura) (severidade ?severidade) (impacto ?impacto) (urgencia ?urgencia)))
   (assert (processar-bug-manual))
)

(defrule processar-bug-manual
   (declare (salience 0))
   (processar-bug-manual)
   ?b <- (BUG (prioridade ?prioridade) (sla ?sla))
   ?c <- (COMPONENTE (nome ?comp-nome) (status ?status-comp))
   =>
   (retract (processar-bug-manual))
   (printout t "\n--- Resultados ---" crlf)
   (printout t "Prioridade do Bug: " ?prioridade crlf)
   (printout t "SLA do Bug: " ?sla crlf)
   (printout t "Sugestao de Status para o Componente " ?comp-nome ": " ?status-comp crlf)

   (open "processamento.log" "a" log-file)
   (printout log-file "\n--- Processamento Manual ---\n")
   (printout log-file "  Bug ID: " (bug-id ?b) ", Prioridade: " ?prioridade ", SLA: " ?sla crlf)
   (printout log-file "  Componente: " ?comp-nome ", Status Sugerido: " ?status-comp crlf)
   (close log-file)
)




(defrule iniciar-processamento-automatico-regras
   (declare (salience 25))
   (operacao-automatica)
   (COMPONENTE ?c)
   (BUG ?b)
   =>
   (printout t "Executando regras de classificacao para processamento automatico..." crlf)
   (run)
)




;; Casos de Teste
;; Para testar, voce pode carregar este arquivo no CLIPS e usar (reset) e (run)
;; Teste de Operacao Automatica (simulando a existencia de todos os arquivos)
;; Para simular, remova os comentarios das linhas abaixo e comente as regras de verificacao de arquivos
;; (assert (arquivo-componentes-existe))
;; (assert (arquivo-catalogo-existe))
;; (assert (arquivo-bugs-existe))
;; (assert (operacao-automatica))

;; Teste de Operacao Manual (simulando a ausencia de arquivos)
;; Para simular, remova os comentarios das linhas abaixo e comente as regras de verificacao de arquivos
;; (assert (operacao-manual))

;; Exemplo de fatos para teste manual (se voce nao quiser digitar)
;; (assert (COMPONENTE (nome "Servico X") (data-implantacao "15/05/2025")))
;; (assert (CATALOGO (componente "Servico X") (impacto-padrao "Localizada") (sla-prioridade-alta "2h") (sla-prioridade-media "4h") (sla-prioridade-baixa "12h")))
;; (assert (BUG (id-bug 10) (componente "Servico X") (assunto "Falha de autenticacao") (data-abertura "08/06/2025") (severidade "Alta Criticidade") (impacto NIL) (urgencia "Alta")))


