(deftemplate temperatura
    (slot dia (type SYMBOL))
    (slot valor (type FLOAT))
)

(deffacts temperatura-iniciais
    (temperatura (dia segunda) (valor 25.0))
    (temperatura (dia terca) (valor 22.5))
)

(defrule adicionar-temperatura
    =>
    (bind ?temp 20.0) ;Simula uma nova leitura (pode ser alterado)
    (if (> ?temp -50) then
        (assert (temperatura (dia quarta) (valor ?temp)))
        (printout t "Temperatura de quarta (" ?temp " øC) registrada" crlf)
     else
        (printout t "Temperatura inv lida: " ?temp " øC" crlf)
    )
)