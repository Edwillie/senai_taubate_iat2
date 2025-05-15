(deftemplate produto
    (slot identificador (type STRING))
    (slot peso (type FLOAT) (default 0.0))
    (slot resistencia (type FLOAT) (default 0.0))
    (slot defeitos-visuais (type INTEGER) (default 0))
)

(deftemplate classificacao
    (slot identificador (type STRING))
    (slot resultado (type STRING))
)

(defrule coletar-dados-produto
    =>
    (printout t "Digite os dados do produto: " crlf)
    (printout t "Identificador: ")
    (bind ?id (read))
    (printout t "Peso: ")
    (bind ?peso (read))
    (printout t "Resistencia: ")
    (bind ?resistencia (read))
    (printout t "Qtde de Defeitos Visuais: ")
    (bind ?defeitos (read))
    (assert (produto    (identificador ?id)
                        (peso ?peso)
                        (resistencia ?resistencia)
                        (defeitos-visuais ?defeitos)
            ) 
    )
)

(defrule rejeitar-peso-invalido
    (declare (salience 20))
    (produto    (identificador ?id)
                (peso ?peso)
                (resistencia ?resistencia)
                (defeitos-visuais ?defeitos)
    )
    (test (not (and (> ?peso 1.0) (< ?peso 2.0))))
    =>
    (assert (classificacao
                (identificador ?id)
                (resultado "Rejeitado pelo peso")
            )   
    )
)

(defrule rejeitar-baixa-resistencia
    (declare (salience 15))
    (produto    (identificador ?id)
                (peso ?peso)
                (resistencia ?resistencia)
                (defeitos-visuais ?defeitos)
    )
    (test (< ?resistencia 500.0))
    =>
    (assert (classificacao
                (identificador ?id)
                (resultado "Rejeitado pela baixa resistˆncia")
            )   
    )
)

(defrule rejeitar-defeito-visual
    (declare (salience 10))
    (produto    (identificador ?id)
                (peso ?peso)
                (resistencia ?resistencia)
                (defeitos-visuais ?defeitos)
    )
    (test (> ?defeitos 2))
    =>
    (assert (classificacao
                (identificador ?id)
                (resultado "Rejeitado pela quantida de de defeitos")
            )   
    )
)

(defrule aprovar-produto
    (declare (salience 5))
    (produto    (identificador ?id)
                (peso ?peso)
                (resistencia ?resistencia)
                (defeitos-visuais ?defeitos)
    )
    (test (and (> ?peso 1.0) (< ?peso 2.0)))
    (test (not (< ?resistencia 500.0)))
    (test (not (> ?defeitos 2)))
    =>
    (assert (classificacao
                (identificador ?id)
                (resultado "Aprovado em todos os crit‚rios")
            )   
    )
)

(defrule processar-classificacao
    (classificacao
        (identificador ?id)
        (resultado ?resultado)
    ) 
    =>
    (printout t "Material: " ?id " foi classificados como: " ?resultado crlf)
)