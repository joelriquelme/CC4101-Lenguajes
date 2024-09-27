#lang play
(require "T2.rkt")

(print-only-errors #t)

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;; Part a)
;; Constructors
(test (Prop? (tt)) #t)
(test (Prop? (ff)) #t)
(test (Prop? (p-not (tt))) #t)
(test (Prop? (p-not (list (tt) (ff)))) #t)
(test (Prop? (p-and (tt))) #t)
(test (Prop? (p-and (list (tt) (ff)))) #t)
(test (Prop? (p-or (tt))) #t)
(test (Prop? (p-or (list (tt) (ff)))) #t)

;; Part b)
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true] )) (p-where (p-id 'x) 'x (tt)))
(test/exn (parse-prop '(or)) "parse-prop: or expects at least two operands")
(test/exn (parse-prop '(or true)) "parse-prop: or expects at least two operands")
(test/exn (parse-prop '(or false)) "parse-prop: or expects at least two operands")
(test/exn (parse-prop '(and)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(and false)) "parse-prop: and expects at least two operands")
(test (parse-prop '(or true (and true false) (false where [x true]))) (p-or (list (tt) (p-and (list (tt) (ff))) (p-where (ff) 'x (tt)))))

;; Part c)
(test (PValue? (ttV)) #t)
(test (PValue? (ffV)) #t)
(test (from-PValue (ttV)) (tt))
(test (from-PValue (ffV)) (ff))

;; Part d)
(test (p-subst (tt) 'x (ff)) (tt))
(test (p-subst (ff) 'x (tt)) (ff))
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-not (p-id 'x)) 'x (ff)) (p-not (ff)))
(test (p-subst (p-or (list (p-id 'x) (p-id 'y))) 'x (ff)) (p-or (list (ff) (p-id 'y))))
(test (p-subst (p-or (list (p-id 'x) (p-id 'y))) 'y (tt)) (p-or (list (p-id 'x) (tt))))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y))) 'x (ff)) (p-and (list (ff) (p-id 'y))))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y))) 'y (tt)) (p-and (list (p-id 'x) (tt))))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-where (p-and (list (p-id 'x) (tt))) 'x (tt)) 'x (ff)) (p-where (p-and (list (p-id 'x) (tt))) 'x (tt)))

(test (p-subst (parse-prop 'true) 'x (ff)) (tt))
(test (p-subst (parse-prop 'false) 'x (tt)) (ff))
(test (p-subst (parse-prop 'x) 'x (tt)) (tt))
(test (p-subst (parse-prop 'x) 'y (tt)) (p-id 'x))
(test (p-subst (parse-prop '(not x)) 'x (ff)) (p-not (ff)))
(test (p-subst (parse-prop '(or x y)) 'x (ff)) (p-or (list (ff) (p-id 'y))))
(test (p-subst (parse-prop '(or x y)) 'y (tt)) (p-or (list (p-id 'x) (tt))))
(test (p-subst (parse-prop '(and x y)) 'x (ff)) (p-and (list (ff) (p-id 'y))))
(test (p-subst (parse-prop '(and x y)) 'y (tt)) (p-and (list (p-id 'x) (tt))))
(test (p-subst (parse-prop '(x where [x true])) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (parse-prop '(x where [y true])) 'x (ff)) (p-where (ff) 'y (tt)))

;; Part e)

;; eval-or
(test (eval-or (list)) (ffV))
(test (eval-or (list (ff) (ff))) (ffV))
(test (eval-or (list (tt) (tt))) (ttV))
(test (eval-or (list (ff) (p-not (tt)) (ff))) (ffV))
(test (eval-or (list (ff) (p-not (ff)) (ff))) (ttV))
(test/exn (eval-or (list (ff) (ff) (p-id 'x) (ff)))
          "Open expression (free occurrence of x)")
(test (eval-or (list (ff) (p-not (ff)) (p-id 'x))) (ttV))

;; eval-and
(test (eval-and (list)) (ttV))
(test (eval-and (list (tt) (ff))) (ffV))
(test (eval-and (list (tt) (tt))) (ttV))
(test (eval-and (list (tt) (p-not (ff)) (tt))) (ttV))
(test (eval-and (list (tt) (p-not (tt)) (tt))) (ffV))
(test/exn (eval-and (list (tt) (tt) (p-id 'x) (ff)))
          "Open expression (free occurrence of x)")
(test (eval-or (list (ff) (p-not (ff)) (p-id 'x))) (ttV))

;; p-eval
(test (p-eval (parse-prop 'true)) (ttV))
(test (p-eval (parse-prop 'false)) (ffV))
(test (p-eval (parse-prop '(not true))) (ffV))
(test (p-eval (parse-prop '(not false))) (ttV))
(test (p-eval (parse-prop '(and true true false))) (ffV))
(test (p-eval (parse-prop '(and true true true))) (ttV))
(test (p-eval (parse-prop '(true where [x true]))) (ttV))
(test (p-eval (parse-prop '(x where [x true]))) (ttV))
(test (p-eval (parse-prop '(x where [x true]))) (ttV))
(test/exn (p-eval (parse-prop '((and x y) where [x true]))) "Open expression (free occurrence of y)")
(test (p-eval (parse-prop '((and x y) where [x false]))) (ffV))
(test/exn (p-eval (parse-prop '((or x y) where [x false]))) "Open expression (free occurrence of y)")
(test (p-eval (parse-prop '((or x y) where [x true]))) (ttV))


;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;; part a)
;; Constructors

(test (Expr? (real 1)) #t)
(test (Expr? (real -1)) #t)
(test (Expr? (real 0)) #t)
(test (Expr? (imaginary 1)) #t)
(test (Expr? (imaginary -1)) #t)
(test (Expr? (imaginary 0)) #t)
(test (Expr? (id 'x)) #t)
(test (Expr? (id 'test)) #t)
(test (Expr? (add 'x 'y)) #t)
(test (Expr? (add 0 0)) #t)
(test (Expr? (add 'x 0)) #t)
(test (Expr? (add 'y 0)) #t)
(test (Expr? (sub 'x 'y)) #t)
(test (Expr? (sub 0 0)) #t)
(test (Expr? (sub 'x 0)) #t)
(test (Expr? (sub 'y 0)) #t)
(test (Expr? (if0 0 'x 'y)) #t)
(test (Expr? (with (list 'x 0) 'x))#t)

;; part b)
(test (parse '1) (real 1))
(test (parse '0) (real 0))
(test (parse '-1) (real -1))
(test (parse '(1 i )) (imaginary 1))
(test (parse '(0 i )) (imaginary 0))
(test (parse '(-1 i )) (imaginary -1))
(test (parse '(+ (2 i ) 1)) (add (imaginary 2) (real 1)))
(test (parse '(+ 1 (2 i ))) (add (real 1) (imaginary 2)))
(test (parse '(- (2 i ) 1)) (sub (imaginary 2) (real 1)))
(test (parse '(- 1 (2 i ))) (sub (real 1) (imaginary 2)))
(test (parse '(with [(x 1) (y 1)] (+ x y))) (with (list (cons 'x(real 1)) (cons 'y(real 1))) (add(id 'x)(id 'y))))
(test (parse '(with [(x y) (y x)] (+ x y))) (with (list (cons 'x (id 'y)) (cons 'y (id 'x))) (add (id 'x) (id 'y))))
(test/exn (parse ' (with [ ] 1)) "parse: 'with' expects at least one definition")
(test (parse '(with [ (x 2) (y (+ x 1))] (+ x y))) (with (list (cons 'x (real 2)) (cons 'y (add (id 'x) (real 1)))) (add (id 'x) (id 'y)))) 

;; part c)
;; Constructor
(test (CValue? (compV 1 1)) #t)
(test (CValue? (compV 0 0)) #t)
(test (CValue? (compV 'x 'y)) #t)

;; from-CValue
(test (from-CValue (compV 0 0)) (real 0))
(test (from-CValue (compV 1 0)) (real 1))
(test (from-CValue (compV -1 0)) (real -1))
(test (from-CValue (compV 0 1)) (imaginary 1))
(test (from-CValue (compV 0 -1)) (imaginary -1))
(test (from-CValue (compV 1 1)) (add (real 1) (imaginary 1)))
(test (from-CValue (compV -1 1)) (add (real -1) (imaginary 1)))
(test (from-CValue (compV 1 -1)) (add (real 1) (imaginary -1)))
(test (from-CValue (compV -1 -1)) (add (real -1) (imaginary -1)))

;; cmplx+
(test (cmplx+ (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx+ (compV 1 1) (compV 0 0)) (compV 1 1))
(test (cmplx+ (compV 0 0) (compV 1 1)) (compV 1 1))
(test (cmplx+ (compV 1 1) (compV 1 1)) (compV 2 2))
(test (cmplx+ (compV -1 -1) (compV 1 1)) (compV 0 0))
(test (cmplx+ (compV 1 1) (compV -1 -1)) (compV 0 0))

;; cmplx-
(test (cmplx- (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx- (compV 1 1) (compV 0 0)) (compV 1 1))
(test (cmplx- (compV 0 0) (compV 1 1)) (compV -1 -1))
(test (cmplx- (compV 1 1) (compV 1 1)) (compV 0 0))
(test (cmplx- (compV -1 -1) (compV 1 1)) (compV -2 -2))
(test (cmplx- (compV 1 1) (compV -1 -1)) (compV 2 2))

;; cmplx0?
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 1 0)) #f)
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 1)) #f)
(test (cmplx0? (cmplx- (compV 1 1) (compV 1 1))) #t)
(test (cmplx0? (cmplx- (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx+ (compV -1 -1) (compV 1 1))) #t)
(test (cmplx0? (cmplx+ (compV 1 1) (compV -1 -1))) #t)
(test (cmplx0? (cmplx+ (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx- (compV 0 1) (compV 1 1))) #f)
(test (cmplx0? (cmplx+ (compV 0 -1) (compV 1 1))) #f)

;; part d)

;; no shadowing
(test (subst (parse '(with [(x 1) (y 1)] (if0 x y z))) 'z (real 1)) (with (list (cons 'x (real 1)) (cons 'y (real 1))) (if0 (id 'x) (id 'y) (real 1))))
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1)) (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x z) (y z)] (+ x z))) 'z (real 1)) (with (list (cons 'x (real 1)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x (2 i)) (y z)] (- x z))) 'z( imaginary 1)) (with (list (cons 'x (imaginary 2)) (cons 'y (imaginary 1))) (sub (id 'x) (imaginary 1))))
(test (subst (parse '(with [(x z) (y z)] (- x z))) 'z (imaginary 1)) (with (list (cons 'x (imaginary 1)) (cons 'y (imaginary 1))) (sub (id 'x) (imaginary 1))))

;; shadowing
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1)) (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (id 'y)) (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))


;; part e)
(test (interp (parse '1)) (compV 1 0))
(test (interp (parse '0)) (compV 0 0))
(test (interp (parse '(1 i))) (compV 0 1))
(test (interp (parse '(+ 1 1))) (compV 2 0))
(test (interp (parse '(+ (1 i) (1 i)))) (compV 0 2))
(test (interp (parse '(+ 1 (1 i)))) (compV 1 1))
(test (interp (parse '(+ 1 (- 2 (1 i))))) (compV 3 -1))
(test/exn (interp (parse '(+ 1 x))) "interp: Open expression (free occurrence of x)")
(test (interp (parse '(if0 0 2 1))) (compV 2 0))
(test (interp (parse '(with [(x 2) (y 3)] (+ y x)))) (compV 5 0))
(test (interp (parse '(with [(x 2) (y 3)] (- x x)))) (compV 0 0))
(test (interp (parse '(with [(x 2) (y 3)] (if0 (- x x) 5 (1 i))))) (compV 5 0))
(test/exn (interp (parse '(if0 (with [(x 2) (y 3)] (- x x)) (- x x) 1 ))) "interp: Open expression (free occurrence of x)")
(test (interp (parse '(if0 (with [(x 2) (y 3)] (- x x)) (- 1 (-1 i)) 1))) (compV 1 1))



























