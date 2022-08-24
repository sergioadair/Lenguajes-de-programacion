#lang racket

; 1.
(define (countdown n)
  (if (= n 0)
      (list n)
      (append (list n) (countdown (sub1 n)) )
      )
  )

; 2.
(define (insertL x y ls)
  (if (eqv? (length ls) 0)
      null
      (if(eqv? (first(cons (first ls) (insertL x y (rest ls)))) x)
         (cons y (cons (first ls) (insertL x y (rest ls))) )
         (cons (first ls) (insertL x y (rest ls)) )
         )
      )
  )

; 3.
(define (remv-1st x ls)
  (if (null? ls)
      null
      (if(equal? x (first ls) )
         (rest ls)
         (cons (first ls) (remv-1st x (rest ls)) )
         )
      )
  )

; 4.
(define (map p ls)
  (if (null? ls)
      null
      (cons (p(first ls)) (map p (rest ls)))
      )
  )

; 5.
(define (filter p ls)
  (if (null? ls)
      null
      (if (p (first ls))
          (cons (first ls) (filter p (rest ls)))
          (filter p (rest ls))
          )
      )
  )

; 6.
(define (zip v u)
  0
  )

; 7.
(define (list-index-ofv v u)
  0
  )

; 9.
(define (reverse ls)
  (if (null? ls)
      null
      (append (reverse (rest ls)) (list (first ls)) )
      )
  )

; 10.
(define (repeat v u)
  0
  )

; 11.
(define (same-lists* v u)
  0
  )

; 13.
(define (binary->natural ls)
  0
  )

; 14.
(define (div v u)
  0
  )

; 15.
(define (append-map v u)
  0
  )

; 16.
(define (set-difference v u)
  0
  )

; 17.
(define (foldr op a ls)
  0
  )

(define (f x ls)
  (if (null? ls)
      null
      (cons (cons x (first ls)) (f x (rest ls)) )
      )
  )

; 18.
(define (powerset ls)
  (if (null? ls)
      (list ls)
      (let ([ps (powerset (rest ls))])
        (append (f (first ls) ps) ps)
        )
      )
  )

; 19.
(define (cartesian-product ls)
  0
  )

; 21.
(define (snowball n)
  0
  )

; 22.
(define (quine v u)
  0
  )



(provide (all-defined-out))
