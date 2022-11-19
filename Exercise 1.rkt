#lang pl

#| 1.a - open-list |#

(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]))

; 1.a - 7 tests:
(test (open-list null) => null) ;sanity check 1 - null
(test (open-list '()) => null) ;sanity check 2 - empty list
(test (open-list '(() () () ())) => null) ;sanity check 3 - list conatining empty lists
(test (open-list '((1 2 3 4))) => '(1 2 3 4)) ;sanity check 3
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)) ; given test
(test (open-list '((1 2 3 4) (5) (6 7) (8 9 10 11 12 13 14 15))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(test (open-list '((1) (#|empty|#) (5) (6 7))) => '(1 5 6 7))

#| 1.b - min&max |#

(: min&max : (Listof (Listof Number)) -> (Listof Number))
(define (min&max lst)
  (let ([lst-open (open-list lst)]) ; https://pl.barzilay.org/lec02#some-style
    (list (+ 0.0 (my_min lst-open +inf.0)) (+ 0.0 (my_max lst-open -inf.0))))) ; (+ 0.0 X) converts 'X' to 'Flonum', thanks to @ofri_tavor

(: my_min : (Listof Number) Number -> Number) ; helper 1 - Minimum
(define (my_min lst n)
  (cond [(null? lst) n]                                     ; returns the last number passed down through recursion - thats the minimum
        [(> n (first lst)) (my_min (rest lst) (first lst))] ; evaluates if n is greater then the first number of the list, and passes that number if its true
        [else (my_min (rest lst) n)]))                      ; otherwise passes the current minimum

(: my_max : (Listof Number) Number -> Number) ; helper 2 - Maximum
(define (my_max lst n)
  (cond [(null? lst) n] ; return the maximum
        [(< n (first lst)) (my_max (rest lst) (first lst))] ; same logic as helper 1, but with the '<' operator instead
        [else (my_max (rest lst) n)]))

; 1.b - 10 tests:
(test (min&max null) => '(+inf.0 -inf.0)) ;sanity check 1 - null
(test (min&max '()) => '(+inf.0 -inf.0)) ;sanity check 2 - empty list
(test (min&max '(() () () ())) => '(+inf.0 -inf.0)) ;sanity check 3 - list conatining empty lists
(test (min&max '((+inf.0) (1 2 3) (-inf.0) (9 2 -1))) => '(-inf.0 +inf.0))
(test (min&max '((+inf.0) (1 2 3) (9 2 -1))) => '(-1.0 +inf.0))
(test (min&max '((1 2 3) (-inf.0) (9 2 -1))) => '(-inf.0 9.0))
(test (min&max '((-2))) => '(-2.0 -2.0))
(test (min&max '((-2 1))) => '(-2.0 1.0))
(test (min&max '((-2) (1))) => '(-2.0 1.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -3) (233 11 90))) => '(-3.0 233.0))

#| 1.c - Apply |#
(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (let ([lst-open (open-list lst)]) ; https://pl.barzilay.org/lec02#some-style
    (cond [(null? lst-open) (list +inf.0 -inf.0)]
          [else (list (apply min lst-open) (apply max lst-open))])))

; 1.c - 10 tests:
(test (min&max_apply null) => '(+inf.0 -inf.0)) ;sanity check 1 - null
(test (min&max_apply '()) => '(+inf.0 -inf.0)) ;sanity check 2 - empty list
(test (min&max_apply '(() () () ())) => '(+inf.0 -inf.0)) ;sanity check 3 - list conatining empty lists
(test (min&max_apply '((+inf.0) (1 2 3) (-inf.0) (9 2 -1))) => '(-inf.0 +inf.0))
(test (min&max_apply '((+inf.0) (1 2 3) (9 2 -1))) => '(-1.0 +inf.0))
(test (min&max_apply '((1 2 3) (-inf.0) (9 2 -1))) => '(-inf.0 9.0))
(test (min&max_apply '((-2))) => '(-2 -2))
(test (min&max_apply '((-2 1))) => '(-2 1))
(test (min&max_apply '((-2) (1))) => '(-2 1))
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))

