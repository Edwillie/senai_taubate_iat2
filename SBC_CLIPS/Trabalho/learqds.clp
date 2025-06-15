; Definindo o template CATALOGO
(deftemplate CATALOGO
  ;"Armazena informa‡äes de neg¢cio para um componente, como SLAs."
  (slot componente (type STRING))
  (slot impacto-padrao (type SYMBOL))
  (slot sla-prioridade-alta (type STRING))
  (slot sla-prioridade-media (type STRING))
  (slot sla-prioridade-baixa (type STRING))
)

; Fun‡Æo para ler o arquivo CSV e carregar os dados
(defrule ler-arquivo-catalogo
    (declare (salience 1000))
    ?f <- (inicio-leitura)
    =>
    (retract ?f)
    (if (open "C:/TEMP/COMPONENTES.CSV" "r")
    then
        (bind ?handle (open "C:/TEMP/COMPONENTES.CSV" "r"))
        (printout t "Carregando dados do arquivo C:/TEMP/COMPONENTES.CSV..." crlf)
        ; Pular cabe‡alho se existir
        (readline ?handle)
        ; Ler cada linha do arquivo
        (while (neq (bind ?linha (readline ?handle)) EOF)
        do
            ; Separar os campos por ponto e v¡rgula
            (bind ?dados (explode$ ";" ?linha))
            (if (>= (length$ ?dados) 5)
            then
                (bind ?componente (nth$ 1 ?dados))
                (bind ?impacto (sym-cat (nth$ 2 ?dados)))
                (bind ?sla-alta (nth$ 3 ?dados))
                (bind ?sla-media (nth$ 4 ?dados))
                (bind ?sla-baixa (nth$ 5 ?dados))
                
                ; Criar fato com os dados lidos
                (assert (CATALOGO
                    (componente ?componente)
                    (impacto-padrao ?impacto)
                    (sla-prioridade-alta ?sla-alta)
                    (sla-prioridade-media ?sla-media)
                    (sla-prioridade-baixa ?sla-baixa)))
            else
                (printout t "Linha inv lida: " ?linha crlf)
            )
        )
        (close ?handle)
        (printout t "Dados carregados com sucesso!" crlf)
    else
        (printout t "Erro ao abrir o arquivo " ?nome-arquivo crlf)
    )
)

; Regra para iniciar o processo de leitura
(defrule iniciar-leitura
    =>
    (assert (inicio-leitura))
)

; Exemplo de como usar:
; (reset)
; (run)