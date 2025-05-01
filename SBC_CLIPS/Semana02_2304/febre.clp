;define template
(deftemplate pessoa
    (slot nome)
    (slot temperatura)
)

(deftemplate doente
    (slot nome)
    (slot temperatura)
)

(deftemplate liberado
    (slot nome)
    (slot temperatura)
)

; define regras
(defrule triagem-doente
    (pessoa (nome ?nome) (temperatura ?temp&:(> ?temp 37.5)))
    =>
    (assert (doente (nome ?nome) (temperatura ?temp)))
    (printout t "O paciente " ?nome " est  com " ?temp " celsius. Caracterizando, Febre!" crlf)
)

; define regras
(defrule triagem-alta
    (pessoa (nome ?nome) (temperatura ?temp&:(<= ?temp 37.5)))
    =>
    (assert (liberado (nome ?nome) (temperatura ?temp)))
    (printout t "O paciente " ?nome " est  com " ?temp " celsius. Deve ser liberado!" crlf)
)

; define os fatos
(deffacts pacientes
    (pessoa (nome "Maria") (temperatura 38))
    (pessoa (nome "JoÆo") (temperatura 37))
    (pessoa (nome "Jos‚") (temperatura 37.8))
    (pessoa (nome "Carla") (temperatura 37.3))
)