(deffacts contador-inicial
    (contador-tarefas 0)
)

(defrule adicionar-tarefa-teclado
    (not (finalizar)) ;S¢ dispara se nÆo houver um fato (finalizar)
    ?f <- (contador-tarefas ?n)
    =>
    (printout t "Digite o nome da tarefa conclu¡da (ou 'sair' para finalizar): ")
    (bind ?tarefa (read))
    (if
        (or (stringp ?tarefa) (symbolp ?tarefa)) then
        (bind ?tarefa-str (if (symbolp ?tarefa) then (str-cat ?tarefa) else ?tarefa))
        (if 
            (eq ?tarefa-str "sair") then
            (assert (finalizar))
            (printout t "Encerrando registro de tarefas. Total de tarefas: " ?n crlf)
         else
            (retract ?f)
            (assert (contador-tarefas (+ ?n 1)))
            (assert (tarefa ?tarefa-str))
            (printout t "Tarefa " ?tarefa " registra. Total de tarefas: " (+ ?n 1) crlf)
        )
     else
        (printout t "Entrada inv lida. Nome da tarefa deve ser texto." crlf) 
    )
)