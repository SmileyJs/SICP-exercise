(load "../util.scm")

(define list-a (list 1 2 3 4))

(display (fold-right / 1 list-a))
(newline)
(display (fold-left / 1 list-a))
(newline)
(display (fold-right list nil list-a))
(newline)
(display (fold-left list nil list-a))