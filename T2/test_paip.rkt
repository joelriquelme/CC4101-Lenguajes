#lang play
(require "T2.rkt")

(print-only-errors #t)


; P1

;; Parte b)
;; Tests para parse-prop
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test/exn (parse-prop '(and)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(or)) "parse-prop: or expects at least two operands")
(test/exn (parse-prop '(or true)) "parse-prop: or expects at least two operands")
(test (parse-prop '((and x true (not (and false (y where [y true])))) where [x (or true false)]))  ;;Test de complejidad
      (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (ff) (p-where (p-id 'y) 'y (tt))))))) 'x (p-or (list (tt) (ff)))))

;; Parte c)
;; Tests para from-PValue
(test (from-PValue (ttV)) (tt))
(test (from-PValue (ffV)) (ff))

;; Parte d)
;; Tests para p-subst

(test (p-subst (tt) 'x (ff)) (tt))
(test (p-subst (ff) 'x (tt)) (ff))
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test  (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-not (p-id 'y)) 'y (ff)) (p-not (ff)))
(test (p-subst (p-or (list (p-id 'x) (p-id 'y) (tt))) 'x (ff)) (p-or (list (ff) (p-id 'y) (tt))))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y) (tt))) 'x (ff)) (p-and (list (ff) (p-id 'y) (tt))))
(test (p-subst (p-or (list (p-where (p-and (list (p-id 'x) (p-not 'y))) 'y (p-and (list (tt) (p-id 'y)))) (p-id 'x) (ff))) 'y (ff)) ;;Test de complejidad
      (p-subst (p-or (list (p-where (p-and (list (p-id 'x) (p-not 'y))) 'y (p-and (list (tt) (ff)))) (p-id 'x) (ff))) 'y (ff)))

#| Tests de integración con parse-prop |#

(test (p-subst (parse-prop 'true) 'x (ff)) (tt))
(test (p-subst (parse-prop 'false) 'x (tt)) (ff))
(test (p-subst (parse-prop 'x) 'x (tt)) (tt))
(test (p-subst (parse-prop 'x) 'y (tt)) (p-id 'x))
(test (p-subst (parse-prop '(x where (x true))) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (parse-prop '(x where (y true))) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (parse-prop '(not y)) 'y (ff)) (p-not (ff)))
(test (p-subst (parse-prop '(or x y true)) 'x (ff)) (p-or (list (ff) (p-id 'y) (tt))))
(test (p-subst (parse-prop '(and x y true)) 'x (ff)) (p-and (list (ff) (p-id 'y) (tt))))



;; Parte e)

#| La función p-eval puede detectar ocurrencias libres, lanzando un error. Lo usaremos para testear la funcionalidad corto-circuito |#

;; Tests para eval-or
(test (eval-or (list (ff) (ff))) (ffV))
(test (eval-or (list (tt) (tt))) (ttV))
(test (eval-or (list (ff) (p-not (p-where (p-id 'x) 'x (tt))) (tt))) (ttV))
(test (eval-or (list (ff) (p-not (p-where (p-id 'x) 'x (tt))) (ff))) (ffV))
(test (eval-or (list (ff) (p-not (p-where (p-id 'x) 'x (tt))) (p-and (list (tt) (p-not (tt)))))) (ffV))
(test/exn (eval-or (list (ff) (p-not (tt)) (p-id 'x) (ff)))
          "Open expression (free occurrence of x)")      ; Se evalua una ocurrencia libre, esperamos error
(test (eval-or (list (ff) (p-not (ff)) (p-id 'x) (ff)))
          (ttV)) ; Lo mismo que antes, pero esta vez se evalua un valor verdadero antes de la occurencia libre: No hay error (corto-circuito)

;; Tests para eval-and
(test (eval-and (list (ff) (ff))) (ffV))
(test (eval-and (list (tt) (tt))) (ttV))
(test (eval-and (list (tt) (p-not (p-where (p-id 'x) 'x (ff))) (tt))) (ttV))
(test (eval-and (list (ff) (p-not (p-where (p-id 'x) 'x (tt))) (ff))) (ffV))
(test (eval-and (list (ff) (p-not (p-where (p-id 'x) 'x (tt))) (p-or (list (ff) (p-not (tt)))))) (ffV))
(test/exn (eval-and (list (tt) (p-not (ff)) (p-id 'x) (ff)))
          "Open expression (free occurrence of x)")      ; Se evalua una ocurrencia libre, esperamos error
(test (eval-and (list (tt) (p-not (tt)) (p-id 'x) (ff)))
          (ffV)) ; Lo mismo que antes, pero esta vez se evalua un valor falso antes que la occurencia libre: No hay error (corto-circuito)

;; Tests para p-eval
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-and (list (tt) (ff) (tt)))) (ffV))
(test (p-eval (p-or (list (tt) (ff)))) (ttV))
(test (p-eval (p-where (ff) 'x (tt))) (ffV))
(test (p-eval (p-where (p-id 'x) 'x (tt))) (ttV))
(test (p-eval (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (ff) (p-where (p-id 'y) 'y (tt))))))) 'x (p-or (list (tt) (ff))))) (ttV))
(test/exn (p-eval (p-id 'x)) "Open expression (free occurrence of x)")
(test/exn (p-eval (p-where (p-id 'y) 'x (tt))) "Open expression (free occurrence of y)")
(test/exn (p-eval (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (tt) (p-where (p-id 'z) 'y (tt)))))))
                       'x (p-or (list (ff) (p-id 'z))))) "Open expression (free occurrence of z)") ; Se evaluan ocurrencias libres, esperamos error
(test (p-eval (p-where (p-and (list (p-id 'x) (tt) (p-not (p-and (list (ff) (p-where (p-id 'z) 'y (tt)))))))
                       'x (p-or (list (tt) (p-id 'z))))) (ttV)) ; Lo mismo que antes, pero p-and y p-or evaluan falso y verdadero (respec.) antes que las
                                                                ;  ocurrencias libres: No hay error (corto-circuito)

#| Tests de integración con parse-prop |#

(test (p-eval (parse-prop '(not true))) (ffV))
(test (p-eval (parse-prop '(and true false true))) (ffV))
(test (p-eval (parse-prop '(or true false))) (ttV))
(test (p-eval (parse-prop '(false where [x true]))) (ffV))
(test (p-eval (parse-prop '(x where [x true]))) (ttV))
(test/exn (p-eval (parse-prop '(and))) "parse-prop: and expects at least two operands")
(test/exn (p-eval (parse-prop '(and true))) "parse-prop: and expects at least two operands")
(test/exn (p-eval (parse-prop '(or))) "parse-prop: or expects at least two operands")
(test/exn (p-eval (parse-prop '(or true))) "parse-prop: or expects at least two operands")
(test (p-eval (parse-prop '((and x true (not (and false (y where [y true])))) where [x (or true false)]))) ;; Test de complejidad
      (ttV))
(test/exn (p-eval (parse-prop 'x)) "Open expression (free occurrence of x)")
(test/exn (p-eval (parse-prop '(y where [x true]))) "Open expression (free occurrence of y)")
(test/exn (p-eval (parse-prop '((and x true (not (and true (z where [y true])))) where [x (or false z)])))
          "Open expression (free occurrence of z)")   ; Se evaluan ocurrencias libres, esperamos error
(test (p-eval (parse-prop '((and x true (not (and false (z where [y true])))) where [x (or true z)]))) 
      (ttV)) ; Lo mismo que antes, ahora hay corto-circuito


; P2

;; Parte b)
;; Tests para parse
(test (parse '1) (real 1))
(test (parse '(1 i )) (imaginary 1))
(test (parse 'x) (id 'x))
(test (parse '(+ 1 (2 i ))) (add (real 1) (imaginary 2)))
(test (parse '(- -1 (-2 i ))) (sub (real -1) (imaginary -2)))
(test (parse '(if0 (+ (1 i) (- 1 2)) (1 i) 1)) (if0 (add (imaginary 1) (sub (real 1) (real 2))) (imaginary 1) (real 1)))
(test (parse '(with [ (x 1) (y 1)] (+ x y))) (with (list (cons 'x ( real 1)) (cons 'y ( real 1))) (add (id 'x) (id 'y))))
(test (parse '(with [ (x 1) (y x)] (+ x y))) (with (list (cons 'x ( real 1)) (cons 'y (id 'x))) (add (id 'x) (id 'y))))
(test/exn (parse '(with [] 1)) "parse: 'with' expects at least one definition")
(test (parse '(with [ (x (1 i)) (y (if0 (+ 0 (-1 i)) (0 i) (- x 1)))] (+ x y)))  ;; Test de complejidad
      (with (list (cons 'x (imaginary 1)) (cons 'y (if0 (add (real 0) (imaginary -1)) (imaginary 0) (sub (id 'x) (real 1))))) (add (id 'x) (id 'y))))

;; Parte c)
;; Tests para from-CValue
(test (from-CValue (compV 0 0)) (real 0))
(test (from-CValue (compV 1 0)) (real 1))
(test (from-CValue (compV 0 2)) (imaginary 2))
(test (from-CValue (compV -5 0)) (real -5))
(test (from-CValue (compV 0 -2)) (imaginary -2))
(test (from-CValue (compV 1 3)) (add (real 1) (imaginary 3)))
(test (from-CValue (compV -1 -4)) (add (real -1) (imaginary -4)))
(test (from-CValue (compV -4 1)) (add (real -4) (imaginary 1)))
(test (from-CValue (compV 1 -4)) (add (real 1) (imaginary -4)))

;; Tests para cmplx+
(test (cmplx+ (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx+ (compV 3 0) (compV 0 2)) (compV 3 2))
(test (cmplx+ (compV 0 2) (compV 3 0)) (compV 3 2))
(test (cmplx+ (compV 0 -2) (compV -3 0)) (compV -3 -2))
(test (cmplx+ (compV 3 1) (compV 2 5)) (compV 5 6))
(test (cmplx+ (compV -3 -1) (compV -2 -5)) (compV -5 -6))
(test (cmplx+ (compV 4 5) (compV -4 -5)) (compV 0 0))

;; Tests para cmplx-
(test (cmplx- (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx- (compV 3 0) (compV 0 2)) (compV 3 -2))
(test (cmplx- (compV 0 2) (compV 3 0)) (compV -3 2))
(test (cmplx- (compV 0 -2) (compV -3 0)) (compV 3 -2))
(test (cmplx- (compV 3 1) (compV 2 5)) (compV 1 -4))
(test (cmplx- (compV -3 -1) (compV -2 -5)) (compV -1 4))
(test (cmplx- (compV 4 5) (compV -4 -5)) (compV 8 10))
(test (cmplx- (compV 4 5) (compV 4 5)) (compV 0 0))

;; Tests para complx0?
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 0)) #f)
(test (cmplx0? (compV 1 1)) #f)
(test (cmplx0? (cmplx+ (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx- (compV 0 0) (compV 0 0))) #t)
(test (cmplx0? (cmplx+ (compV -1 -2) (compV -3 -3))) #f)
(test (cmplx0? (cmplx- (compV 3 2) (compV 4 1))) #f)
(test (cmplx0? (cmplx+ (compV 3 2) (compV -3 -2))) #t)
(test (cmplx0? (cmplx- (compV 3 2) (compV 3 2))) #t)


;; Parte d)
;; Tests para subst-bindings

#| Casos sin shadowing |#

(test (subst-bindings (list) 'z (real 1)) (cons (list) #f))
(test (subst-bindings (list (cons 'x (real 2)) (cons 'y (id 'z))) 'z (real 1))
      (cons (list (cons 'x (real 2)) (cons 'y (real 1))) #f))
(test (subst-bindings (list (cons 'x (real 2)) (cons 'y (id 'z))) 'w (imaginary 1))
      (cons (list (cons 'x (real 2)) (cons 'y (id 'z))) #f))
(test (subst-bindings (list (cons 'x (real 2)) (cons 'y (id 'w)) (cons 'z (add (id 'w) (if0 (sub (real 1) (id 'w)) (real 1) (imaginary 1))))) 'w (real 1))
      (cons (list (cons 'x (real 2)) (cons 'y (real 1)) (cons 'z (add (real 1) (if0 (sub (real 1) (real 1)) (real 1) (imaginary 1))))) #f)) ;; Test de complejidad

#| Casos con shadowing |#

(test (subst-bindings (list (cons 'z (real 0))) 'z (real 1)) (cons (list (cons 'z (real 0))) #t))
(test (subst-bindings (list (cons 'x (real 1)) (cons 'x (imaginary 1))) 'x (real 2))
      (cons (list (cons 'x (real 1)) (cons 'x (imaginary 1))) #t))
(test (subst-bindings (list (cons 'z (real 2)) (cons 'y (id 'z))) 'z (real 1))
      (cons (list (cons 'z (real 2)) (cons 'y (id 'z))) #t))
(test (subst-bindings (list (cons 'x (id 'w)) (cons 'w (id 'z))) 'w (imaginary 1))
      (cons (list (cons 'x (imaginary 1)) (cons 'w (id 'z))) #t))
(test (subst-bindings (list (cons 'x (id 'w)) (cons 'w (id 'z)) (cons 'z (add (id 'w) (if0 (sub (real 1) (id 'w)) (real 1) (imaginary 1))))) 'w (real 1))
      (cons (list (cons 'x (real 1)) (cons 'w (id 'z)) (cons 'z (add (id 'w) (if0 (sub (real 1) (id 'w)) (real 1) (imaginary 1))))) #t)) ;; Test de complejidad


;; Tests para subst

#| Casos sin shadowing |#

(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(+ x y)) 'x (real 10))
      (add (real 10) (id 'y)))
(test (subst (parse '(if0 x 1 0)) 'x (real 5))
      (if0 (real 5) (real 1) (real 0)))
(test (subst (parse '(+ (- x 3) y)) 'x (real 2))
      (add (sub (real 2) (real 3)) (id 'y)))
(test (subst (parse '(with [(x 2) (y (+ x z))] (+ x y))) 'z (real 5))
      (with (list (cons 'x (real 2)) (cons 'y (add (id 'x) (real 5)))) (add (id 'x) (id 'y))))

#| Casos con shadowing |#

(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))
(test (subst (parse '(with [(x 2)] (with [(y x)] (+ y x)))) 'x (real 3))
      (with (list (cons 'x (real 2))) (with (list (cons 'y (id 'x))) (add (id 'y) (id 'x)))))
(test (subst (parse '(if0 x (with [(x 2)] (+ x y)) z)) 'x (real 4))
      (if0 (real 4) (with (list (cons 'x (real 2))) (add (id 'x) (id 'y))) (id 'z)))
(test (subst (parse '(with [(x 2)] (+ x (with [(x 3)] (+ x y))))) 'x (real 5))
      (with (list (cons 'x (real 2))) (add (id 'x) (with (list (cons 'x (real 3))) (add (id 'x) (id 'y))))))


;; Parte e)
;; Tests para interp
(test (interp (real 5)) (compV 5 0))
(test (interp (imaginary 3)) (compV 0 3))
(test (interp (add (real 5) (imaginary 3))) (compV 5 3))
(test (interp (sub (imaginary 7) (imaginary 2))) (compV 0 5))
(test (interp (add (sub (real 10) (imaginary 3)) (add (real 4) (imaginary 1)))) (compV 14 -2))
(test (interp (with (list (cons 'x (real 2)) (cons 'y (imaginary 5))) (add (id 'x) (id 'y)))) (compV 2 5))
(test (interp (with (list (cons 'x (real 3))) (with (list (cons 'x (imaginary 4))) (add (id 'x) (id 'x))))) (compV 0 8))
(test (interp (if0 (real 0) (imaginary 7) (real 5))) (compV 0 7))
(test (interp (if0 (real 0) (with (list (cons 'x (imaginary 3))) (add (id 'x) (real 2))) (id 'x))) (compV 2 3))
(test (interp (with (list (cons 'x (real 1)) (cons 'x (real 2))) (id 'x))) (compV 2 0))  ; sobre-escritura
(test/exn (interp (id 'y)) "Open expression (free occurrence of y)")

#| Tests de integración con parse |#

(test (interp (parse '5)) (compV 5 0))
(test (interp (parse '(5 i))) (compV 0 5))
(test (interp (parse '(+ 5 (5 i)))) (compV 5 5))
(test (interp (parse '(- (5 i) (2 i)))) (compV 0 3))
(test (interp (parse '(+ (- 10 (3 i)) (+ 4 (1 i))))) (compV 14 -2))
(test (interp (parse '(with ((x 2) (y (5 i))) (+ x y)))) (compV 2 5))
(test (interp (parse '(with ((x 3)) (with ((x (4 i))) (+ x x))))) (compV 0 8))
(test (interp (parse '(if0 0 (5 i) 5))) (compV 0 5))
(test (interp (parse '(if0 0 (with ((x (3 i))) (+ x 2)) x))) (compV 2 3))
(test (interp (parse '(with ((x 1) (x 2)) x))) (compV 2 0))  ; sobre-escritura
(test/exn (interp (parse 'z)) "Open expression (free occurrence of z)")
