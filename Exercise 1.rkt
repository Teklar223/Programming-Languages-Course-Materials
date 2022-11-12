#lang pl

#| 1.a - open-list |#

(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]))

; 1.a - 5 tests:
(test (open-list null) => null) ;sanity check
(test (open-list '()) => null) ;sanity check 2
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)) ; given test
(test (open-list '((1 2 3 4) (5) (6 7) (8 9 10 11 12 13 14 15))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(test (open-list '((1) ( ) (5) (6 7))) => '(1 5 6 7)) ; can handle 'null' in the form of an empty list - ( )

#| 1.b - min&max |#


; 1.b - tests: