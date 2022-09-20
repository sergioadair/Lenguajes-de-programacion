#lang plait

(require "arithlang.rkt")
(print-only-errors #t)

(test (eval `(+ 1 2)) 3)
(test (eval `(+ 1 -2)) -1)
(test (eval `(+ -1 2)) 1)
(test (eval `(+ -1 -2)) -3)
(test (eval `(+ 1 0)) 1)
(test/exn (eval `(+ 1 a)) "expresión aritmética malformada")

(test (eval `(* 1 2)) 2)
(test (eval `(* 1 -2)) -2)
(test (eval `(* -1 2)) -2)
(test (eval `(* -1 -2)) 2)
(test (eval `(* 1 0)) 0)
(test/exn (eval `(* a 1)) "expresión aritmética malformada")

(test (eval `(- 1 2)) -1)
(test (eval `(- 1 -2)) 3)
(test (eval `(- -1 2)) -3)
(test (eval `(- -1 -2)) 1)
(test (eval `(- 1 0)) 1)
(test/exn (eval `(- 1 a)) "expresión aritmética malformada")

(test (eval `(- 1)) -1)
(test (eval `(- 0)) 0)
(test (eval `(- 0)) -0)
(test (eval `(- -1)) 1)
(test (eval `(- (- 1))) 1)
(test (eval `(- (- (- 1)))) -1)
(test/exn (eval `(- a)) "expresión aritmética malformada")

(test/exn (eval `(/ 1 2)) "operación aritmética malformada")
(test/exn (eval `hola) "expresión aritmética malformada")
