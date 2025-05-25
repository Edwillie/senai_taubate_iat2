(deftemplate peca
    (slot id (type STRING))
    (slot tolerancia (type FLOAT) (default 0.0))
    (slot resistencia (type FLOAT) (default 0.0))
)

(deftemplate status-producao
    (slot id (type STRING))
    (slot resultado (type STRING))
)

(defrule verificar-qualidade
    (peca (id ?id)
          (tolerancia ?tol&:(<= ?tol 0.01))
          (resistencia ?res&:(>= ?res 500.0)) 
    )
    =>
    (assert (status-producao (id ?id) (resultado "aprovada")))
)

(defrule processar-status
    (status-producao (id ?id) (resultado "aprovada"))
    =>
    (printout t "Pe‡a " ?id " aprovada para montagem." crlf)
)

; (assert (peca (id "P003") (tolerancia 0.008) (resistencia 520.0)))
; (assert (peca (id "P004") (tolerancia 0.015) (resistencia 480.0)))