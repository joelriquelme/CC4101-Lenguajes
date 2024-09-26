#lang play
(require "T2.rkt")

(print-only-errors #t)

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;; Part a)
;; Cronstructors
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






















