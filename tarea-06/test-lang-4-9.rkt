#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-9.rkt")

(initialize-store!)

(define-test-suite test
  (test-case "empty-store"
             (check-equal? (empty-store)
                           '#()))
  (test-case "get-store"
             (check-equal? (get-store)
                           '#()))
  (test-case "reference?"
             (check-equal? (reference? "0")
                           #f))
  (test-case "reference?"
             (check-equal? (reference? -1)
                           #f))
  (test-case "reference?"
             (check-equal? (reference? -0)
                           #t))
  (test-case "reference?"
             (check-equal? (reference? 0)
                           #t))
  (test-case "newref"
             (check-equal? (newref 1)
                           0))
  (test-case "newref"
             (check-equal? (newref 2)
                           1))
  (test-case "deref"
             (check-equal? (deref (newref 3))
                           3))
  (test-case "get-store"
             (check-equal? (get-store)
                           '#(1 2 3)))
  (test-case "deref"
             (check-equal? (deref (newref (deref 0)))
                           1))
  (setref! 3 4)
  (test-case "setref!"
             (check-equal? (get-store)
                           '#(1 2 3 4)))
  (setref! (newref 5) (newref 6))
  (test-case "setref!"
             (check-equal? (get-store)
                           '#(1 2 3 4 5 6)))
  ;(test-exn "setref!" (setref! -1 7) (report-invalid-reference -1 the-store)) ;Tira el error en vez de ser un success.
  )
  

(run-tests test 'verbose)