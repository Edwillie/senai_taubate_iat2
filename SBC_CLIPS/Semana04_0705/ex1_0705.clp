(deftemplate peca
    (slot identificador (type STRING))
    (slot tolerancia (type FLOAT) (default 0.0))
    (slot resistencia (type FLOAT) (default 0.0))
    (slot peso (type FLOAT) (default 0.0))
)

(defrule verificar-qualidade
    (peca   (identificador ?id)
            (tolerancia ?tol&:(<= ?tol 0.01))
            (resistencia ?res&:(>= ?res 500.0))
            (peso ?peso&:(and (>= ?peso 1.0) (<= ?peso 2.0)))
    )
    =>
    (printout t "Pe‡a " ?id " atende aos padräes de qualidade. Tolerancia: " ?tol " mm, Resistˆncia: " ?res " MPa, Peso: " ?peso " kg." crlf)
)

; (assert (peca (identificador "P001") (tolerancia 0.005) (resistencia 550.0) (peso 1.5)))
; (assert (peca (identificador "P002") (tolerancia 0.02) (resistencia 400.0) (peso 1.8)))