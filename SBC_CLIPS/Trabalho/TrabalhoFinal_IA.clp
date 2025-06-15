;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUGS - Sistema Especialista para Priorização de Bugs em aplicações
;; Autor....: Edwillie Cardoso
;; Instrutor: Marcos Richetto
;; Data.....: 2025-06-11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------------
;; Definição dos Templates e Globais
;; ------------------------------------------------------------------
;"Representa um produto, serviço ou recurso da empresa."
(deftemplate COMPONENTE
  (slot nome (type STRING))
  (slot data-implantacao (type STRING))
  (slot status (type SYMBOL) (default EM_OPERACAO))
)

;"Armazena informações de negócio para um componente, como SLAs."
(deftemplate CATALOGO
  (slot componente (type STRING))
  (slot impacto-padrao (type SYMBOL))
  (slot sla-prioridade-alta (type INTEGER))
  (slot sla-prioridade-media (type INTEGER))
  (slot sla-prioridade-baixa (type INTEGER))
)

;"Representa um bug reportado para um componente."
(deftemplate BUG
  (slot id-bug (type INTEGER))
  (slot componente (type STRING))
  (slot assunto (type STRING))
  (slot data-abertura (type STRING))
  (slot severidade (type SYMBOL))
  (slot impacto (type SYMBOL))
  (slot urgencia (type SYMBOL))
  (slot prioridade (type SYMBOL))
  (slot sla_tempo_min (type INTEGER))
)

;; ------------------------------------------------------------------
;; Definição de Fatos
;; ------------------------------------------------------------------
;"Status de controle de fase do programa"
(deffacts status-gerais
   (fase-programa ler-arquivos)
   (modelo-execucao definir)
   (contador 0)
)

;; ------------------------------------------------------------------
;; Declaração de Funções
;; ------------------------------------------------------------------
;"Checa a existencia dos arquivos para processamento"
(deffunction verificar-arquivo (?nome-arquivo)
   (if (open ?nome-arquivo arquivo "r")
      then
         (close arquivo)
         TRUE
      else
         FALSE
   )
)

;"Função para dividir uma string com base em um delimitador"
(deffunction split-string (?texto-lin ?separador)
  (bind ?result (create$))
  (bind ?ctrloop TRUE)
  (bind ?trecho ?texto-lin)
  (while ?ctrloop
    (bind ?start 1)
    ( if (str-index ?separador ?trecho) then
        (bind ?end (str-index ?separador ?trecho))
        (bind ?result (create$ ?result (sub-string ?start (- ?end 1) ?trecho)))
        (bind ?start (+ ?end 1))
	      (bind ?trecho (sub-string ?start (str-length ?trecho) ?trecho))
      else
        (bind ?result (create$ ?result ?trecho))
        (bind ?ctrloop FALSE)
    )
  )
  (return ?result)
)

;; ------------------------------------------------------------------
;; Declaração de Regras e Ações do Programa
;; ------------------------------------------------------------------
;"regra 01 - Verifica se os arquivos existem"
(defrule verifica-arquivos
    ?f <- (fase-programa ler-arquivos)
    => 
    (printout t "**********************************************************************************************" crlf)
    (printout t "***** BUGS - CLASSIFICAÇÃO DE CHAMADOS: INICIANDO PROGRAMA                               *****" crlf)
    (printout t "**********************************************************************************************" crlf)
    ( if (verificar-arquivo "C:/TEMP/COMPONENTES.CSV") then
        (assert (chk-file-componentes ok))
      else
        (assert (chk-file-componentes nok))
    )
    ( if (verificar-arquivo "C:/TEMP/CATALOGOS.CSV") then
        (assert (chk-file-catalogos ok))
      else
        (assert (chk-file-catalogos nok))
    )
    ( if (verificar-arquivo "C:/TEMP/BUGS.CSV") then
        (assert (chk-file-bugs ok))
      else
        (assert (chk-file-bugs nok))
    )
    (bind ?file-name "C:/TEMP/BUGS_CLASSIFICADOS.CSV")
    (remove ?file-name)
    (bind ?file-log "C:/TEMP/PROCESSING.LOG")
    (remove ?file-log)
    (assert (fase-programa verifica-automacao))
    (retract ?f)
)  

;"regra 02 - Inicia execução Manual"
(defrule execucao-manual
  ?f <- (fase-programa verifica-automacao)
  ?m <- (modelo-execucao definir)
  (chk-file-componentes nok) 
  (chk-file-catalogos nok)  
  (chk-file-bugs nok) 
  =>
  (printout t crlf "Fase 1: Entrada de dados - Execucao Manual - Arquivos não foram encontrados em C:\\TEMP" crlf)
  (printout t crlf "Fase 1.1: Entrada de dados - Identificando o COMPONENTE" crlf)
  (printout t ">>> Informe o COMPONENTE (ou digite sair para encerrar o programa): ")
  (bind ?comp (read))
  (if (or (eq ?comp sair) (eq ?comp "sair")) then
      (printout t "**********************************************************************************************" crlf)
      (printout t "***** BUGS - FIM DO PROCESSAMENTO MANUAL                                                 *****" crlf)
      (printout t "**********************************************************************************************" crlf) 
      (assert (fase-programa encerrar-programa))
    else
      (assert (comp-digitado ?comp))
      (assert (fase-programa verificar-existencia))   
  )
  (assert (modelo-execucao manual))
  (retract ?f ?m)
)

;"regra 03 - Inicia execução Automática"
(defrule execucao-automatica
  ?f <- (fase-programa verifica-automacao)
  ?m <- (modelo-execucao definir)
  (chk-file-componentes ok)
  (chk-file-catalogos ok)
  (chk-file-bugs ok)
  =>
  (printout t "Fase 1: Entrada de dados - Execucao ** AUTOMATICA ** - Arquivos encontrados em C:\\TEMP" crlf)
  (assert (fase-programa carga-automatica-componentes))
  (assert (modelo-execucao automatico))
  (retract ?f ?m)
)

;; ------------------------------------------------------------------
;; Entrada: Automatização por arquivos
;; ------------------------------------------------------------------
;"regra 04 - Automatização: Leitura de dados de COMPONENTES"
(defrule read-componente-csv
  ?f <- (fase-programa carga-automatica-componentes)
  =>
  (printout t crlf "Fase 1.1: Entrada de dados - Lendo Arquivo COMPONENTES.CSV ... " crlf)  
  (open "C:/TEMP/COMPONENTES.CSV" csvfile "r")  ;Abrir o arquivo
  (readline csvfile)                            ;Lê a primeira linha sem fazer nada. Ignorar a primeira linha (cabeçalho)
  (bind ?line (readline csvfile))               ;Ler cada linha do arquivo
  (while (neq ?line EOF)
    (printout t ">>> COMPONENTES (LINHA): " ?line crlf)
    (bind ?fields (split-string ?line ";"))     ;Dividir a linha em campos usando o delimitador ";"
    (assert (COMPONENTE                         ;Criar fato COMPONENTE com os valores lidos
                (nome (nth$ 1 ?fields))
                (data-implantacao (nth$ 2 ?fields))
                (status (sym-cat (nth$ 3 ?fields)))
            )
    )
    (bind ?line (readline csvfile)) ;Ler próxima linha
  )
  (close csvfile) ;Fechar o arquivo
  (assert (fase-programa carga-automatica-catalogos))
  (retract ?f)
)

;"regra 05 - Automatização: Leitura de dados de CATALOGO"
(defrule read-catalogo-csv
  ?f <- (fase-programa carga-automatica-catalogos)
  =>
  (printout t crlf "Fase 1.2: Entrada de dados - Lendo Arquivo CATALOGOS.CSV ... " crlf)
  (open "C:/TEMP/CATALOGOS.CSV" csvfile "r")    ;Abrir o arquivo
  (readline csvfile)                            ;Lê a primeira linha sem fazer nada. Ignorar a primeira linha (cabeçalho)
  (bind ?line (readline csvfile))               ;Ler cada linha do arquivo
  (while (neq ?line EOF)
    (printout t ">>> CATALOGOS (LINHA): " ?line crlf)
    (bind ?fields (split-string ?line ";"))     ;Dividir a linha em campos usando o delimitador ";"
    (assert (CATALOGO                           ;Criar fato CATALOGO com os valores lidos
                (componente (nth$ 1 ?fields))
                (impacto-padrao (sym-cat (nth$ 2 ?fields)))
                (sla-prioridade-alta (nth$ 3 ?fields))
                (sla-prioridade-media (nth$ 4 ?fields))
                (sla-prioridade-baixa (nth$ 5 ?fields))
            )
    )
    (bind ?line (readline csvfile)) ;Ler próxima linha
  )
  (close csvfile) ;Fechar o arquivo
  (assert (fase-programa carga-automatica-bugs))
  (retract ?f)
)

;"regra 06 - Automatização: Leitura de dados de BUGS"
(defrule read-bugs-csv
  ?f <- (fase-programa carga-automatica-bugs)
  =>
  (printout t crlf "Fase 1.3: Entrada de dados - Lendo Arquivo BUGS.CSV ... " crlf)
  (open "C:/TEMP/BUGS.CSV" csvfile "r")         ;Abrir o arquivo
  (readline csvfile)                            ;Lê a primeira linha sem fazer nada. Ignorar a primeira linha (cabeçalho)
  (bind ?line (readline csvfile))               ;Ler cada linha do arquivo
  (while (neq ?line EOF)
    (printout t ">>> BUGS (LINHA): " ?line crlf)
    (bind ?fields (split-string ?line ";"))     ;Dividir a linha em campos usando o delimitador ";"
    (assert (BUG                                ;Criar fato BUGS com os valores lidos
                (id-bug (nth$ 1 ?fields))
                (componente (nth$ 2 ?fields))
                (assunto (nth$ 3 ?fields))
                (data-abertura (nth$ 4 ?fields))
                (severidade (upcase (sym-cat (nth$ 5 ?fields) ) ) ) 
                (impacto (upcase (sym-cat (nth$ 6 ?fields) ) ) )
                (urgencia (upcase (sym-cat (nth$ 7 ?fields) ) ) )
                (prioridade (upcase (sym-cat (nth$ 8 ?fields) ) ) )
                (sla_tempo_min (nth$ 9 ?fields))
            )
    )
    (bind ?line (readline csvfile)) ;Ler próxima linha
  )
  (close csvfile) ;Fechar o arquivo
  (assert (fase-programa definir-prioridade))
  (retract ?f)
)

;; ------------------------------------------------------------------
;; Entrada: Operacionalização por Input do Usuário
;; ------------------------------------------------------------------
;"regra 07 - Operacionalização: Cadastro do Novo Componente e Catalogo de Suporte"
(defrule novo-componente
    ?f <- (fase-programa verificar-existencia)
    ?v <- (comp-digitado ?comp)
    (not (COMPONENTE (nome ?comp)))
    =>
    ;Input do cadastro do novo componente
    (printout t crlf ">>> Registrando novo COMPONENTE (" ?comp ") <<<:" crlf)
    (printout t ">>> Informe a data de Implantação (YYYY-MM-DD): ")
    (bind ?cdi (read))
    (printout t ">>> Informe Status do Componente (NOVO, EM_OPERACAO, DECOMISSIONADO, PROBLEMATICO): ")
    (bind ?csts (upcase (read)))
    ;Input do catalogo de suporte para o novo componente
    (printout t crlf "Fase 1.2: Entrada de dados - Definindo o CATALOGO" crlf)
    (printout t ">>> Informe o IMPACTO (INDIVIDUAL, LOCALIZADO, AMPLO, EXTERNO): ")
    (bind ?cimp (upcase (read)))
    (printout t crlf ">>> Informe o SLA (Tempo Acordado de Atendimento - MINUTOS): " crlf)
    (printout t ">>>>>>> Tempo para ALTA prioridade: ")
    (bind ?tsla-alta (read))    
    (printout t ">>>>>>> Tempo para MEDIA prioridade: ")
    (bind ?tsla-media (read))   
    (printout t ">>>>>>> Tempo para BAIXA prioridade: ")
    (bind ?tsla-baixa (read)) 
    ;Registra os dados nos templates         
    (assert (COMPONENTE (nome ?comp)
                        (status ?csts)
                        (data-implantacao ?cdi)) 
            (CATALOGO (componente ?comp)
                      (impacto-padrao ?cimp)
                      (sla-prioridade-alta ?tsla-alta)
                      (sla-prioridade-media ?tsla-media)
                      (sla-prioridade-baixa ?tsla-baixa))         
    )
    (printout t "***** NOVO COMPONENTE E CATALOGO REGISTRADOS! *****" crlf)
    (assert (fase-programa registrar-bug))
    (retract ?f)
)

;"regra 08 - Operacionalização: Redirecionamento para o cadastro do Bug"
(defrule componente-existe
    ?f <- (fase-programa verificar-existencia)
    ?v <- (comp-digitado ?comp)
    (COMPONENTE (nome ?comp))
    =>
    (assert (fase-programa registrar-bug))
    (retract ?f)
)

;"regra 09 - Operacionalização: Input do Bug"
(defrule registrar-bug
    ?f <- (fase-programa registrar-bug)
    (COMPONENTE (nome ?comp))
    =>
    ;Input do novo bug
    (printout t crlf "Fase 1.3: Entrada de dados - Input de dados para o BUG" crlf)
    (printout t ">>> Registrando novo *** BUG *** para o COMPONENTE (" ?comp ") <<<:" crlf)    
    (printout t ">>>>>>> Informe um ID para o bug: ")
    (bind ?bugId (read))  
    (printout t ">>>>>>> Informe o ASSUNTO para o bug: ")
    (bind ?bugSubject (read))
    (printout t ">>>>>>> Informe o DATA DE OCORRENCIA do bug (YYYY-MM-DD): ")
    (bind ?bugDate (read))    
    (printout t ">>>>>>> Informe o SEVERIDADE do bug (ALTA, BAIXA): ")
    (bind ?bugSev (upcase (read)))  
    (printout t ">>>>>>> Informe o IMPACTO do bug (INDIVIDUAL, LOCALIZADO, AMPLO, EXTERNO): ")
    (bind ?bugImp (upcase (read)))  
    (printout t ">>>>>>> Informe o URGENCIA do bug (ALTA, MEDIA, BAIXA): ")
    (bind ?bugUrg (upcase (read)))                    
    (assert  (BUG  (id-bug ?bugId)
                   (componente ?comp)
                   (assunto ?bugSubject)
                   (data-abertura ?bugDate)
                   (severidade ?bugSev)
                   (impacto ?bugImp)
                   (urgencia ?bugUrg)
                   (prioridade PEND))
    )
    (assert (fase-programa definir-prioridade))
    (retract ?f)
)

;"regra 10: Verificar se todos os bugs foram classificados"
(defrule verificar-todos-classificados
   (declare (salience 10))
   ?f <- (fase-programa definir-prioridade)
   (not (BUG (prioridade PEND)))
   =>
   (printout t "Todos os bugs foram classificados!" crlf)
   (assert (fase-programa controle-fluxo))
   (retract ?f)
)

;"regra 11: Alertar bugs não classificados"
(defrule alertar-bugs-nao-classificados
   (declare (salience 5))
   ?f <- (fase-programa definir-prioridade)
   (BUG (id-bug ?id) (prioridade PEND))
   =>
   (printout t "Aviso: Bug ID " ?id " não foi classificado (valores inválidos ou sem correspondência nas regras)." crlf)
   (assert (fase-programa controle-fluxo))
   (retract ?f)
)
;; ------------------------------------------------------------------
;; Processamento: Classificação dos Bugs e Status do Componente de TI
;; ------------------------------------------------------------------
;"regra 12: Classificacao para Prioridade ALTISSIMA (Salience: 100) - Maior prioridade de execução"
(defrule prioridade-altissima
   (declare (salience 100))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade ALTA) 
                (urgencia ?urg&:(or (eq ?urg ALTA) (eq ?urg MEDIA)))
                (impacto ?imp&:(or (eq ?imp AMPLO) (eq ?imp EXTERNO))) 
            )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-alta ?sla)
   )
   =>
   (modify ?bug (prioridade ALTISSIMA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade ALTISSIMA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 13: Classificacao para Prioridade ALTA (Salience: 90)"
(defrule prioridade-alta
   (declare (salience 90))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade BAIXA) 
                (urgencia ?urg&:(or (eq ?urg ALTA) (eq ?urg MEDIA)))
                (impacto ?imp&:(or (eq ?imp AMPLO) (eq ?imp EXTERNO))) 
            )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-alta ?sla)
   )
   =>
   (modify ?bug (prioridade ALTA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade ALTA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 14: Classificacao para Prioridade MEDIA_ALTA (Salience: 80)"
(defrule prioridade-media-alta
   (declare (salience 80))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade ALTA)
                (urgencia ALTA) 
                (impacto LOCALIZADO) 
          )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-media ?sla)
   )
   =>
   (modify ?bug (prioridade MEDIA_ALTA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade MEDIA_ALTA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 15: Classificacao para Prioridade MEDIA_BAIXA (Salience: 70)"
(defrule prioridade-media-baixa
   (declare (salience 70))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade BAIXA) 
                (urgencia ?urg&:(or (eq ?urg MEDIA) (eq ?urg BAIXA)))
                (impacto LOCALIZADO) 
          )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-media ?sla)
   )
   =>
   (modify ?bug (prioridade MEDIA_BAIXA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade MEDIA_BAIXA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 16: Classificacao para Prioridade BAIXA (Salience: 60)"
(defrule prioridade-baixa
   (declare (salience 60))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade ALTA) 
                (urgencia ?urg&:(or (eq ?urg ALTA) (eq ?urg MEDIA)))
                (impacto INDIVIDUAL) 
          )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-media ?sla)
   )
   =>
   (modify ?bug (prioridade BAIXA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade BAIXA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 17: Classificacao para Prioridade BAIXISSIMA (Salience: 50) - Menor prioridade de execução"
(defrule prioridade-baixissima
   (declare (salience 50))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (severidade BAIXA) 
                (urgencia ?urg&:(or (eq ?urg ALTA) (eq ?urg MEDIA) (eq ?urg BAIXA)))
                (impacto INDIVIDUAL) 
          )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-baixa ?sla)
   )
   =>
   (modify ?bug (prioridade BAIXISSIMA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade BAIXISSIMA para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;"regra 18: Classificacao para Prioridade BAIXISSIMA (Salience: 50) - Menor prioridade de execução"
(defrule sem-classificacao
   (declare (salience 30))
   ?f <- (fase-programa definir-prioridade)
   ?bug <- (BUG (id-bug ?id) 
                (componente ?comp) 
                (prioridade PEND)
          )
   (CATALOGO (componente ?comp) 
             (sla-prioridade-baixa ?sla)
   )
   =>
   (modify ?bug (prioridade NENHUMA) (sla_tempo_min ?sla))
   (printout t "Definindo prioridade NENHUMA (SEM CLASSIFICACAO) para o bug " ?id " (SLA: " ?sla " minutos)" crlf)
)

;; ------------------------------------------------------------------
;; Controle de Fluxo
;; ------------------------------------------------------------------
;"regra 19: Controle de fluxo de processo manual (loop)"
(defrule controle-fluxo-manual
   ?f <- (fase-programa controle-fluxo)
   ?m <- (modelo-execucao manual)
   =>
   (printout t "**********************************************************************************************" crlf)
   (printout t "***** BUGS - CLASSIFICAÇÃO DE CHAMADOS: RETORNO DE FLUXO                                 *****" crlf)
   (printout t "**********************************************************************************************" crlf)
   (assert (fase-programa verifica-automacao))
   (assert (modelo-execucao definir))
   (retract ?f ?m)
)

;"regra 20: Controle de fluxo de processo automatico (encerramento)"
(defrule controle-fluxo-automatico
   ?f <- (fase-programa controle-fluxo)
   ?m <- (modelo-execucao automatico)
   =>
   (printout t "**********************************************************************************************" crlf)
   (printout t "***** BUGS - FIM DO PROCESSAMENTO AUTOMATICO                                             *****" crlf)
   (printout t "**********************************************************************************************" crlf)
   (assert (fase-programa encerrar-programa))
   (retract ?f)
)

;; ------------------------------------------------------------------
;; Saida: Atualização dos Arquivos e Criação do Log
;; ------------------------------------------------------------------
;"regra 21: Grava o arquivo com os bugs classificados"
(defrule gravar-bugs-classificados
    ?f <- (fase-programa encerrar-programa)
    (BUG  (id-bug ?id) 
          (componente ?comp) 
          (assunto ?assunto) 
          (data-abertura ?data) 
          (severidade ?sev) 
          (impacto ?imp) 
          (urgencia ?urg) 
          (prioridade ?prio) 
          (sla_tempo_min ?sla)
    )
    ?c <- (contador ?n)
    =>
    (bind ?file-name "C:/TEMP/BUGS_CLASSIFICADOS.CSV")
    (open ?file-name outfile "a")
    (if (eq ?n 0) then
      (retract ?c)
      (printout outfile "id_bug;componente;assunto;data_abertura;severidade;impacto;urgencia;prioridade;sla_tempo_min" crlf)
      (assert (contador (+ ?n 1)))
     else
      (printout outfile ?id ";" ?comp ";" ?assunto ";" ?data ";" ?sev ";" ?imp ";" ?urg ";" ?prio ";" ?sla crlf)
    )

    (close outfile)
)

;"regra 22: Grava o arquivo de log da operação"
(defrule gravar-log-operacao
   ?f <- (fase-programa encerrar-programa)
   (BUG   (id-bug ?id) 
          (componente ?comp) 
          (assunto ?assunto) 
          (data-abertura ?data) 
          (severidade ?sev) 
          (impacto ?imp) 
          (urgencia ?urg) 
          (prioridade ?prio) 
          (sla_tempo_min ?sla)
   )
   =>
   (bind ?file-log "C:/TEMP/PROCESSING.LOG")
   (open ?file-log logfile "a")
   (printout logfile "Bug ID: " ?id " | Componente: " ?comp " | Assunto: " ?assunto " | Data Abertura: " ?data " | Severidade: " ?sev " | Impacto: " ?imp " | Urgencia: " ?urg " | Prioridade: " ?prio " | SLA: " ?sla " minutos" crlf)
   (close logfile)
)

