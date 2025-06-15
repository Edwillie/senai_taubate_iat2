;; Define o deftemplate CATALOGO (como vocˆ j  forneceu)
(deftemplate CATALOGO
  ;"Armazena informa‡äes de neg¢cio para um componente, como SLAs."
  (slot componente (type STRING))
  (slot impacto-padrao (type SYMBOL))
  (slot sla-prioridade-alta (type STRING))
  (slot sla-prioridade-media (type STRING))
  (slot sla-prioridade-baixa (type STRING))
)

;;; A fun‡Æo 'load-csv-to-catalogo' lˆ um arquivo CSV e cria fatos CATALOGO.
;;; Parƒmetros:
;;;   ?filename: O nome do arquivo CSV a ser lido.
(deffunction load-csv-to-catalogo (?filename)
  (bind ?file-handle (open ?filename "r"))
  (if (eq ?file-handle FALSE) then
    (printout t "Erro: NÆo foi poss¡vel abrir o arquivo " ?filename crlf)
    (return FALSE)
  )

  (while (not (eof ?file-handle))
    (bind ?line (readline ?file-handle))
    (if (not (eq ?line "")) then
      (bind ?fields (str-explode ?line ";")) ; Divide a linha por ponto e v¡rgula

      ;; Verifica se h  o n£mero correto de campos
      (if (eq (length ?fields) 5) then
        (make-instance of CATALOGO
          (componente (nth$ 1 ?fields))
          (impacto-padrao (str-to-symbol (nth$ 2 ?fields))) ; Converte a string para SYMBOL
          (sla-prioridade-alta (nth$ 3 ?fields))
          (sla-prioridade-media (nth$ 4 ?fields))
          (sla-prioridade-baixa (nth$ 5 ?fields))
        )
      else
        (printout t "Aviso: Linha ignorada devido a n£mero incorreto de campos: " ?line crlf)
      )
    )
  )
  (close ?file-handle)
  (printout t "Fatos CATALOGO carregados com sucesso do arquivo " ?filename crlf)
  (return TRUE)
)

; Regra para testar a leitura
(defrule test-read-csv
  =>
  (load-csv-to-catalogo "C:/TEMP/COMPONENTES.CSV")
  )
;;; Exemplo de uso:
;;; Para usar, salve o c¢digo acima em um arquivo .clp e, em seguida,
;;; no prompt do CLIPS, vocˆ pode chamar a fun‡Æo:
;;;
;;; (load-csv-to-catalogo "seu_arquivo.csv")
;;;
;;; Certifique-se de que "seu_arquivo.csv" exista no mesmo diret¢rio
;;; onde vocˆ est  executando o CLIPS ou forne‡a o caminho completo.

;;; Exemplo de conte£do para "seu_arquivo.csv":
;;;
;;; Componente A;ALTA;4h;8h;24h
;;; Componente B;MEDIA;8h;16h;48h
;;; Componente C;BAIXA;24h;48h;72h
;;; Outro Componente;ALTA;2h;4h;12h