(deffacts contador-inicial
    (contador 0)
)

(defrule adicionar-item-contado
    ?f <- (contador ?n)
    (test (< ?n 3))
    =>
    (retract ?f)
    (assert (contador (+ ?n 1)))
    (assert (item "item" (+ ?n 1)))
    (printout t "Item " (+ ?n 1) " adicionado. Total "(+ ?n 1) crlf)
)