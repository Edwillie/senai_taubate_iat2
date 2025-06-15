; Defini‡Æo do template CATALOGO
(deftemplate CATALOGO
  ;"Armazena informa‡äes de neg¢cio para um componente, como SLAs."
  (slot componente (type STRING))
  (slot impacto-padrao (type SYMBOL))
  (slot sla-prioridade-alta (type STRING))
  (slot sla-prioridade-media (type STRING))
  (slot sla-prioridade-baixa (type STRING))
)

; Fun‡Æo para dividir uma string com base em um delimitador
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

; Fun‡Æo para ler e processar o arquivo CSV
(deffunction read-csv (?filename)
  (open ?filename csvfile "r")    ;Abrir o arquivo
  (readline csvfile)              ;Lˆ a primeira linha sem fazer nada. Ignorar a primeira linha (cabe‡alho)
  (bind ?line (readline csvfile)) ;Ler cada linha do arquivo
  (while (neq ?line EOF)
    (bind ?fields (split-string ?line ";")) ;Dividir a linha em campos usando o delimitador ";"
    (assert (CATALOGO                       ;Criar fato CATALOGO com os valores lidos
                (componente (nth$ 1 ?fields))
                (impacto-padrao (sym-cat (nth$ 2 ?fields)))
                (sla-prioridade-alta (nth$ 3 ?fields))
                (sla-prioridade-media (nth$ 4 ?fields))
                (sla-prioridade-baixa (nth$ 5 ?fields))
            )
    )
    (bind ?line (readline csvfile)) ;Ler pr¢xima linha
  )
  (close csvfile) ;Fechar o arquivo
)

; Regra para testar a leitura
(defrule test-read-csv
  =>
  (read-csv "C:/TEMP/COMPONENTES.CSV")
  (facts)
)

  ;(open "C:/TEMP/COMPONENTES.CSV" csvfile "r")
  ;(bind ?line (readline csvfile))
  ;(printout t ?line crlf)

  ;(bind ?delimiter ";")
  ;(bind ?start 1)
  ;(bind ?trecho ?line)
  ;(bind ?end (str-index ";" ?trecho))

  ;(bind ?result (create$))
  ;(bind ?result (create$ ?result (sub-string ?start (- ?end 1) ?trecho)))
  ;(bind ?start (+ ?end 1))


  ;(bind ?end (str-index ";" (sub-string ?start (str-length ?line) ?line)))
  ;(bind ?trecho (sub-string ?start (str-length ?line) ?line))
 