#lang pl

#| 1.a - open-list |#

(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]))

; 1.a - tests:
(test (open-list null) => null)
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))

#| 1.b - min&max |#


; 1.b - tests: