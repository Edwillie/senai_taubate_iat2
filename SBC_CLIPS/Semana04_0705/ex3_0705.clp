(deftemplate sensor
    (slot id (type STRING))
    (slot temperatura (type FLOAT) (default 0.0))
    (slot pressao (type FLOAT) (default 0.0))
)

(defrule coletar-dados-sensor
    =>
    (printout t "Digite o ID do sensor: ")
    (bind ?id (read))
    (printout t "Digite a temperatura (C): ")
    (bind ?temp (read))
    (printout t "Digite a pressÆo (bar): ")
    (bind ?press (read))
    (assert (sensor (id ?id) (temperatura ?temp) (pressao ?press)))
)

(defrule classificar-temperatura
    (sensor (id ?id) (temperatura ?temp))
    =>
    (if (>= ?temp 100.0) then
        (printout t "Sensor " ?id ": temperatura cr¡tica (" ?temp ")." crlf)
     else
        (if (>= ?temp 50.0) then
            (printout t "Sensor " ?id ": temperatura normal (" ?temp ")." crlf)
         else
            (printout t "Sensor " ?id ": temperatura baixa (" ?temp ")." crlf)
        )
    )
)