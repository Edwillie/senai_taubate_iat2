(deftemplate sensor
    (slot id (type STRING))
    (slot temperatura (type FLOAT) (default 0.0))
)

(defrule alerta-temperatura-critica
    (sensor (id ?id) (temperatura ?temp&:(>= ?temp 100.0)))
    =>
    (printout t "Sensor " ?id ": ALERTA CRITICO - Temperatura " ?temp "C!" crlf)
)

(defrule alerta-temperatura-elevada
    (sensor (id ?id) (temperatura ?temp&:(and (>= ?temp 80.0) (< ?temp 100.0))))
    =>
    (printout t "Sensor " ?id ": ALERTA - Temperatura Elevada " ?temp "C!" crlf)
)

(defrule alerta-temperatura-normal
    (sensor (id ?id) (temperatura ?temp&:(< ?temp 80.0)))
    =>
    (printout t "Sensor " ?id ": Temperatura Normal " ?temp "C!" crlf)
)

; (set-strategy depth)
; (assert (sensor (id "S002") (temperatura 110.0)))
; (assert (sensor (id "S003") (temperatura 85.0)))
; (assert (sensor (id "S004") (temperatura 60.0))) 

; (set-strategy breadth)
; (assert (sensor (id "S002") (temperatura 110.0)))
; (assert (sensor (id "S003") (temperatura 85.0)))
; (assert (sensor (id "S004") (temperatura 60.0)))