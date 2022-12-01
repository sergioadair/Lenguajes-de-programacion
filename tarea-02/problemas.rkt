#lang racket

(require pict)
(require racket/draw)
(require pict/color)

(define (unit-string? x)
  (and (string? x)
      (= (string-length x) 1)))

(define(unit-string-list? x)
  (or (null? x)
     (and (pair? x)
         (string? (first x))
         (= (string-length (first x)) 1)
         (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s) (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

(define (take l n)
  (unless (list? l) (error 'take "esperaba una lista pero recibí: ~e" l))
  (unless (number? n) (error 'take "esperaba un número pero recibí: ~e" n))
  (cond
    [(< n 0) (error 'take "n no puede ser menor que 0")]
    [(>= n (length l)) l]
    [(= n 0) '()]
    [else (cons (first l) (take (rest l) (- n 1)))]))

(define (drop l n)
  (unless (list? l) (error 'drop "esperaba una lista pero recibí: ~e" l))
  (unless (number? n) (error 'drop "esperaba un número pero recibí: ~e" n))
  (cond
    [(< n 0) (error 'drop "n no puede ser menor que 0")]
    [(>= n (length l)) '()]
    [(= n 0) (cons (first l) (drop (rest l) n))]
    [else (drop (rest l) (- n 1))]))

(define (list->chunks l n)
  (unless (list? l) (error 'list->chunks "esperaba una lista pero recibí: ~e" l))
  (unless (number? n) (error 'list->chunks "esperaba un número pero recibí: ~e" n))
  (when (< n 1) (error 'list->chunks "n no puede ser menor que 1") )
  (cond
    [(null? l) null]
    [else
     (cons (take l n)
           (list->chunks (drop l n) n))]))

(define (bundle s n)
  (when (< n 1) (error 'bundle "n no puede ser menor que 1") )
  (cond
    [(null? s) null]
    [else
     (if (unit-string-list? s)
         (cons (implode (take s n))
          (bundle (drop s n) n))
         (list->chunks s n))]))

(define (partition s n)
  (unless (string? s) (error 'partition "esperaba una cadena, pero recibí: ~e" s))
  (unless (number? n) (error 'partition "esperaba un número pero recibí: ~e" n))
  (cond
    [(< n 1) (error 'partition "n no puede ser menor que 1")]
    [(= (string-length s) 0) null]
    [(< (string-length s) n) (list s)]
    [else (cons (substring s 0 n) (partition (substring s n) n))]))


(define (isort ls pred)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) pred)
              pred)))

(define (insert n ls pred)
  (cond
    [(eq? pred "as")
     (cond
       [(empty? ls) (list n)]
       [(<= n (first ls)) (cons n ls)]
       [else (cons (first ls) (insert n (rest ls) pred))])]
    [(eq? pred "des")
     (cond
       [(empty? ls) (list n)]
       [(>= n (first ls)) (cons n ls)]
       [else (cons (first ls) (insert n (rest ls) pred))])]))

(define (smallers ls pivot)
  (cond
    [(null? ls) ls]
    [(< (first ls) pivot) (cons (first ls) (smallers (rest ls) pivot))]
    [else (smallers (rest ls) pivot)]))

(define (largers ls pivot)
  (cond
    [(null? ls) ls]
    [(> (first ls) pivot) (cons (first ls) (largers (rest ls) pivot))]
    [else (largers (rest ls) pivot)]))

(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (list pivot)
             (quicksort (largers ls pivot)))]))

(define (equals ls pivot)
  (cond
    [(null? ls) ls]
    [(= (first ls) pivot) (cons pivot (equals (rest ls) pivot))]
    [else (equals (rest ls) pivot)]))

(define (quicksort-fixed ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort-fixed (smallers ls pivot))
             (equals ls pivot)
             (quicksort-fixed (largers ls pivot)))]))

(define (quicksort-any ls proc)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (quicksort-any (filter (lambda (x) (proc x pivot)) ls) proc)
               (filter (lambda (x) (equal? x pivot)) ls)
               (quicksort-any (filter (lambda (x) (and (not (proc x pivot)) (not (equal? x pivot)))) ls) proc))]))

(define (smallers-filter ls n)
  (filter (lambda (x)
            (> n x)) ls))

(define (largers-filter ls n)
  (filter (lambda (x)
            (> x n)) ls))

(define (quicksort-local ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort-local (filter (lambda (x) (< x pivot)) ls))
             (equals ls pivot)
             (quicksort-local (filter (lambda (x) (> x pivot)) ls)))]))

(define (mosaic side c)
  (vc-append (c(arrowhead 10 (* 3 (/ pi 2))))
             (vc-append (hc-append (c(arrowhead 10 0))
                                   (white (arrowhead 10 0))
                                   (c(arrowhead 10 pi)))
                        (c(arrowhead 10 (/ pi 2))))))

(define (umbrellacorp side c)
  (cond [(<= side 10) (mosaic side c) ]
        [else
         (define center (umbrellacorp (/ side 2) c))
         (define center-white (umbrellacorp (/ side 2) white))
         (vc-append center (vc-append (hc-append center center-white center) center)) ]))


(provide (all-defined-out))

