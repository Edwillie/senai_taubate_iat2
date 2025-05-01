(deftemplate estudante
    (slot nome (type STRING))
    (slot nota (type FLOAT) (default 0.0))
)

(deffacts estudantes-iniciais
    (estudante (nome "JoÆo") (nota 7.5))
    (estudante (nome "Maria") (nota 8.0))
)

(defrule adicionar-estudante
    =>
    (assert (estudante (nome "Ana") (nota 9.0)))
    (printout t "Estudante Ana adicionado com nota 9.0" crlf)
)