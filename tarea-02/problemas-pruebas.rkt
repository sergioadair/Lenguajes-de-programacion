#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas

  (test-case "bundle1"
    (check-equal? (bundle (explode "abcdefgh") 1)
                  '("a" "b" "c" "d" "e" "f" "g" "h")))
  
  (test-case "bundle2"
    (check-equal? (bundle (explode "abcdefgh") 2)
                  '("ab" "cd" "ef" "gh")))

  (test-case "bundle3"
    (check-equal? (bundle (explode "abcdefg") 3)
                  '("abc" "def" "g")))

  (test-case "bundle4"
    (check-equal? (bundle '("a" "b") 3)
                  '("ab")))

  (test-case "bundle5"
    (check-equal? (bundle '() 3)
                  '()))

  (check-exn exn:fail? (thunk (bundle '("a" "b" "c") 0)))

  (test-case "take1"
    (check-equal? (take '(1 2 3) 1)
                  '(1)))
  
  (test-case "take2"
    (check-equal? (take '(1 2 3) 4)
                  '(1 2 3)))

  (test-case "take3"
    (check-equal? (take '(1 2 3) 0)
                  '()))

  (check-exn exn:fail? (thunk (take '(1 2 3) -1)))
  (check-exn exn:fail? (thunk (take "(1 2 3)" 1)))
  (check-exn exn:fail? (thunk (take '(1 2 3) "1")))

  (test-case "drop1"
    (check-equal? (drop '(1 2 3) 1)
                  '(2 3)))
  
  (test-case "drop2"
    (check-equal? (drop '(1 2 3) 4)
                  '()))

  (test-case "drop3"
    (check-equal? (drop '(1 2 3) 0)
                  '(1 2 3)))

  (check-exn exn:fail? (thunk (drop '(1 2 3) -1)))
  (check-exn exn:fail? (thunk (drop "(1 2 3)" 1)))
  (check-exn exn:fail? (thunk (drop '(1 2 3) "1")))

  (test-case "list->chunks1"
    (check-equal? (list->chunks '(1 2 3 4) 2)
                  '((1 2) (3 4))))
  
  (test-case "list->chunks2"
    (check-equal? (list->chunks '(1 2 3) 2)
                  '((1 2) (3))))

  (test-case "list->chunks3"
    (check-equal? (list->chunks '(1 2 3) 4)
                  '((1 2 3))))
  
  (test-case "list->chunks6"
    (check-equal? (list->chunks '() 2)
                  '()))

  (check-exn exn:fail? (thunk (list->chunks '(1 2 3) 0)))
  (check-exn exn:fail? (thunk (list->chunks "(1 2 3)" 1)))
  (check-exn exn:fail? (thunk (list->chunks '(1 2 3) "1")))

  (test-case "partition1"
    (check-equal? (partition "abcdefgh" 1)
                  (bundle (explode "abcdefgh") 1)))
  
  (test-case "partition2"
    (check-equal? (partition "abcdefgh" 2)
                  '("ab" "cd" "ef" "gh")))

  (test-case "partition3"
    (check-equal? (partition "abcdefg" 3)
                  '("abc" "def" "g")))

  (test-case "partition4"
    (check-equal? (partition "ab" 3)
                  '("ab")))

  (test-case "partition5"
    (check-equal? (partition "" 3)
                  '()))

  (check-exn exn:fail? (thunk (partition "abc" 0)))
  (check-exn exn:fail? (thunk (partition '("a" "b" "c") 1)))
  (check-exn exn:fail? (thunk (partition "abc" "1")))
  
  )

(run-tests pruebas 'verbose)