(deftemplate temperatura
    (slot dia (type SYMBOL))
    (slot valor (type FLOAT))
)

(defrule adicionar-temperatura-teclado
    => 
    (printout t "Digite o dia (ex.: segunda, terca): ")
    (bind ?dia (read))
    (printout t "Digite a temperatura (øC): ")
    (bind ?temp (read))
    (if
        (and (symbolp ?dia) (numberp ?temp) (>= ?temp -50.0) (<= ?temp 50.0)) then
        (assert (temperatura (dia ?dia) (valor ?temp)))
        (printout t "Temperatura de " ?dia " (" ?temp " øC) registrada." crlf)
     else
        (printout t "Entrada inv lida. Dia deve ser um s¡mbolo e temperatura entre -50.0 e 50.0." crlf)
    )
)