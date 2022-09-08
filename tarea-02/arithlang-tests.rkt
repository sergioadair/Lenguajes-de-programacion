#lang plait

(require "arithlang.rkt")
(print-only-errors #t)

(test (eval `(+ 1 2)) 3)
(test (eval `(+ 1 -2)) -1)
(test (eval `(+ -1 2)) 1)
(test (eval `(+ -1 -2)) -3)
(test (eval `(+ 1 0)) 1)

(test (eval `(* 1 2)) 2)
(test (eval `(* 1 -2)) -2)
(test (eval `(* -1 2)) -2)
(test (eval `(* -1 -2)) 2)
(test (eval `(* 1 0)) 0)

(test (eval `(- 1 2)) -1)
(test (eval `(- 1 -2)) 3)
(test (eval `(- -1 2)) -3)
(test (eval `(- -1 -2)) 1)
(test (eval `(- 1 0)) 1)

(test (eval `(~ 1)) -1)
(test (eval `(~ 0)) 0)
(test (eval `(~ 0)) -0)
(test (eval `(~ -1)) 1)
(test (eval `(~ (~ 1))) 1)
(test (eval `(~ (~ (~ 1)))) -1)

(test (eval `(/ 1 2)) 0) ; Aqui dará error 'parse "operación aritmética malformada"
(test (eval `hola) 0)    ; Aqui dará error 'parse "expresión aritmética malformada"
