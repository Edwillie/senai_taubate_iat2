(defrule adicionar-item-teclado
    =>
    (printout t "Digite o nome do item: ")
    (bind ?nome (read))
    (printout t "Digite a quantidade: ")
    (bind ?quantidade (read))
    (if (and (or (stringp ?nome) (symbolp ?nome)) (numberp ?quantidade) (> ?quantidade 0)) then
        (bind ?nome-str (if (symbolp ?nome) then (str-cat ?nome) else ?nome))
        (assert (item ?nome-str ?quantidade))
        (printout t "Item " ?nome " com quantidade " ?quantidade " adicionado" crlf)
     else
        (printout t "Entrada inv lida. Nome deve ser texto e quantidade um n£mero positivo." crlf)
    )
)