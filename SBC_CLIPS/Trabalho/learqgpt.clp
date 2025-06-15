(deftemplate CATALOGO
  "Armazena informa‡äes de neg¢cio para um componente, como SLAs."
  (slot componente (type STRING))
  (slot impacto-padrao (type SYMBOL))
  (slot sla-prioridade-alta (type STRING))
  (slot sla-prioridade-media (type STRING))
  (slot sla-prioridade-baixa (type STRING))
)

;; Fun‡Æo que separa uma string com base em um delimitador ";"
(deffunction split-by-semicolon (?line)
  (bind ?result (create$))
  (bind ?start 1)
  (bind ?end (str-index ";" ?line))
  (while ?end
    (bind ?field (sub-string ?start (- ?end 1) ?line))
    (bind ?result (create$ ?result ?field))
    (bind ?start (+ ?end 1))
    (bind ?end (str-index ";" ?line ?start))
  )
  (bind ?last (sub-string ?start (str-length ?line) ?line))
  (bind ?result (create$ ?result ?last))
  ?result
)

;; Regra para ler e carregar dados do arquivo
(defrule carregar-catalogo-do-csv
  (declare (salience 100))
  ?start <- (fase leitura-csv)
  =>
  (printout t "Lendo dados do arquivo catalogo.csv..." crlf)
  (bind ?file (open "C:/TEMP/COMPONENTES.CSV" "r"))
  (if (neq ?file FALSE) then
    (while (bind ?linha (readline ?file))
      (if (neq ?linha "") then
        (bind ?campos (split-by-semicolon ?linha))
        (assert (CATALOGO
          (componente (nth$ 1 ?campos))
          (impacto-padrao (sym-cat (nth$ 2 ?campos)))
          (sla-prioridade-alta (nth$ 3 ?campos))
          (sla-prioridade-media (nth$ 4 ?campos))
          (sla-prioridade-baixa (nth$ 5 ?campos))
        ))
      )
    )
    (close ?file)
    (printout t "CATALOGO carregado com sucesso!" crlf)
  else
    (printout t "Erro ao abrir o arquivo catalogo.csv" crlf)
  )
  (retract ?start)
)

;; In¡cio da execu‡Æo
(reset)
(assert (fase leitura-csv))
(run)
