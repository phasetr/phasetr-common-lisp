(begin
  (load "~/junk/lisp/The_Little_Prover/scheme/j-bob-lang.scm")
  (load "~/junk/lisp/The_Little_Prover/scheme/j-bob.scm")
  (load "~/junk/lisp/The_Little_Prover/scheme/little-prover.scm"))
(let ((lst '(2 1 0)))
  (print (car lst))
  (print (cdr lst))
  (print (cons 1 0)))

;; P.3
(car (cons 'ham '(eggs)))
(car (cons 'ham '(cheese)))
(car (cdr (cons 'eggs '(ham))))
(car (cons (car '(ham)) '(eggs)))
(car (cons 'ham '(eggs)))
(atom (cons 'ham '(eggs)))
;; P.4
;;(atom (cons a b))
;; P.5
;; (equal 'flapjack (atom (cons a b)))
(equal 'flapjack 'nil)
;; P.7
(atom '())

;; P.8
;; The Axioms of Cons (initial
(dethm atom/cons (x y)
       (equal (atom (cons x y)) 'nil))
(dethm car/cons (x y)
       (equal (car (cons x y)) x))
(dethm cdr/cons (x y)
       (equal (cdr (cons x y)) y))
(equal 'eggs '(ham))
;; P.9
;;(car
;; (cons (equal (cons x y) (cons x y))
;;       '(and crumpets)))

;; P.10
;; The Axioms of Equal (initial)
(dethm equal-same (x)
       (equal (equal x x) 't))
(dethm equal-swap (x y)
       (equal (equal x y) (equal y x)))

