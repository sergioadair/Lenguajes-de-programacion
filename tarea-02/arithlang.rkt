#lang plait

(define-type ArithC
   (numC [n : Number])
   (plusC [l : ArithC] [r : ArithC])
   (multC [l : ArithC] [r : ArithC]))

(define-type ArithS
  (numS [n : Number])
  (plusS [l : ArithS] [r : ArithS])
  (multS [l : ArithS] [r : ArithS])
  (subS [l : ArithS] [r : ArithS])
  (minusS [e : ArithS])
  )

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

(define (interp [a : ArithC]) : Number
  (type-case ArithC a
    [(numC n) n]
    [(plusC l r) (+ (interp l)(interp r) )]
    [(multC l r) (* (interp l)(interp r) )]
    )
  )

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l)(desugar r))]
    [(multS l r) (multC (desugar l)(desugar r))]
    [(subS l r) (plusC (desugar l)(multC (numC -1) (desugar r)) )]
    [(minusS e) (multC (desugar e)(numC -1))]
    )
  )

(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(+) (plusS (parse (second ls)) (parse (third ls)))]
             [(*) (multS (parse (second ls)) (parse (third ls)))]
             [(-) (subS (parse (second ls)) (parse (third ls)))]
             [(~) (minusS (parse (second ls)))]
             [else (error 'parse "operación aritmética malformada")]))]
        [else (error 'parse "expresión aritmética malformada")]
        )
  )


