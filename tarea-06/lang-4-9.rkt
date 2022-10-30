#lang racket

(define (empty-store) (vector))

(define the-store 'uninitialized)

(define (get-store) the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (and (integer? v)
       (not (negative? v))))

(define (newref val)
  (let ((next-ref (vector-length the-store)))
    (set! the-store (vector-append the-store (vector val)))
    next-ref ))

(define (deref ref )
  (vector-ref the-store ref ))

(define (report-invalid-reference ref the-store)
  (error 'setref "referencia no válida ~e en almacenamiento ~e" ref the-store))

(define (setref! ref val)
  (vector-set! the-store
               (if (and (reference? ref)
                        (< ref (vector-length the-store)))
                   ref
                   (report-invalid-reference ref the-store))
               val))

(provide (all-defined-out))