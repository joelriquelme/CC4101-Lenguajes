#lang play

#|

Hizo Ud uso de la whiteboard policy: SI
En caso que afirmativo, indique con quién y sobre qué ejercicio:
- Carlos Ruz
- Ejercicio 2

|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and <prop>)
         | (p-or <prop>)
         | (p-where <prop> <sym> <prop>)
         | (p-id <sym>)
Prop represents a Boolean values and operators.
|#

(deftype Prop
  (tt)
  (ff)
  (p-not p)
  (p-and ps)
  (p-or ps)
  (p-id x)
  (p-where body x name-expr))


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'not <s-prop>)
          | (list 'and <s-prop>)
          | (list 'or <s-prop>)
          | (list '<sym> where (list <sym> <s-prop>)
          | <sym>
|#

;; parse-prop : <s-prop> -> Prop
;; Parses lenguaje of Boolean propositions.
(define (parse-prop s-prop)
  (match s-prop
    ['true (tt)]
    ['false (ff)]
    [(? symbol? x) (p-id x)]
    [(list 'not sprop) (p-not (parse-prop sprop))]
    [(list 'and sprops ...)
     (if (< (length sprops) 2)
         (error "parse-prop: and expects at least two operands")
         (p-and (map parse-prop sprops)))]
    [(list 'or sprops ...)
     (if (< (length sprops) 2)
         (error "parse-prop: or expects at least two operands")
         (p-or (map parse-prop sprops)))]
    [(list body 'where (list (? symbol? x) sprop))
     (p-where (parse-prop body) x (parse-prop sprop))]
    ))

;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ttV
          | ffV
PValue is a recursive data type that represents the notion of the values in the language.
|#

(deftype PValue
  (ttV)
  (ffV))

;; from-Pvalue : PValue -> Prop
;; Convert a PValue in to Prop element
(define (from-PValue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; This function do the substitution of a proposition with an identifier.
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id x)
     (if (equal? x name)
         substitution
         (p-id x))]
    [(p-not sprop)   
     (p-not (p-subst sprop name substitution))]  
    [(p-and sprops) 
     (p-and (map (λ (sprops) (p-subst sprops name substitution)) sprops))]
    [(p-or sprops) 
     (p-or (map (λ (sprops) (p-subst sprops name substitution)) sprops))]
    [(p-where b x e)
     (p-where (if (equal? x name)
                  b
                  (p-subst b name substitution)
              )
              x
              (p-subst e name substitution))] 
   ))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
;; Recursive evaluation of p-or with short-circuiting if a ttV is found in any proposition.
(define (eval-or ps)
  (match ps
    [(list) (ffV)]
    [(list l-prop r-prop ...)
     (if (equal? (p-eval l-prop) (ffV))
         (eval-or r-prop)
         (ttV))]
  ))

;; eval-and : (Listof Prop) -> PValue
;; Recursive evaluation of p-and with short-circuiting if a ffV is found in any proposition.
(define (eval-and ps)
  (match ps
    [(list) (ttV)]
    [(list l-prop r-prop ...)
     (if (equal? (p-eval l-prop) (ttV))
         (eval-and r-prop)
         (ffV))]
  ))

;; p-eval : Prop -> PValue
;; Recursive evaluation of a Prop, returns a PValue.
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-not sprop)
     (match (p-eval sprop)
         [(ttV) (ffV)]
         [(ffV) (ttV)]
     )
    ]
    [(p-and sprops) (eval-and sprops)]
    [(p-or sprops) (eval-or sprops)]
    [(p-where b x e)
     (p-eval (p-subst b x (from-PValue (p-eval e))))
    ]
    [(p-id x) (error 'p-eval "Open expression (free occurrence of ~a)" x)]
 ))

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::=(real <num>)
        | (imaginary <num>)
        | (id <sym>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (with (list <sym> <expr>) <expr>)

expr represents a complex number
|#
(deftype Expr
  (real n)
  (imaginary n)
  (id x)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with vars expr)
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (list <num> 'i)
        | <sym>
        | (list '+ <s-expr> <s-expr>)
        | (list '- <s-expr> <s-expr>)
        | (list 'if0 <s-expr> <s-expr> <s-expr>)
        | (list 'with (list <sym> <s-expr>) <s-expr>)


s-expr represents a complex number in concrete syntax
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr)
  (match s-expr
    [(? number? n) (real n)]
    [(list (? number? n) 'i) (imaginary n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]
    [(list 'with vars sexprs)
     (if (< (length vars) 1)
         (error 'parse "'with' expects at least one definition")
         (with (map (λ (vars) (def (list (? symbol? var) sexpr) vars) (cons var (parse sexpr))) vars) ;; (list <sym> <s-expr>)
               (parse sexprs)) ;; <s-expr>
     )
    ]
  )
 )

;;----- ;;
;; P2.c ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
CValue respresents a complex number, the first value is a real and the second is a imaginary.
|#
(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; transform a CValue in to Expr (abstract).
(define (from-CValue v)
  (def(compV r i) v)
  (cond
    [(zero? i) (real r)]
    [(zero? r) (imaginary i)]
    [(add (real r) (imaginary i))]
   )
 )

;; cmplx+ :: CValue CValue -> CValue
;; add two complex numbers.
(define (cmplx+ v1 v2)
  (def (compV r1 i1) v1)
  (def (compV r2 i2) v2)
  (compV (+ r1 r2) (+ i1 i2))
  )

;; cmplx- :: CValue CValue -> CValue
;; sub two complex numbers.
(define (cmplx- v1 v2)
  (def (compV r1 i1) v1)
  (def (compV r2 i2) v2)
  (compV (- r1 r2) (- i1 i2))
  )

;; cmplx0? :: CValue -> Boolean
;; returns true if the complex number is zero.
(define (cmplx0? v)
  (def (compV r i) v)
  (if (and (zero? r) (zero? i))
      #t
      #f)
  )

;;----- ;;
;; P2.d ;;
;;----- ;;

;;subst-in :: ListOf[(Symbol Expr)] Symbol Expr ListOf[Symbol] -> ListOf[(Symbol Expr)]
;; this function returns a list of vars, check if the var is shadowed or not and replace it.
(define (subst-in original-vars what for vars)
  (match original-vars
    ['() '()] ;; Base case
    [(list f-var r-vars ...)
     (def (cons symbol expr) f-var) ;; get the first element of list of original vars
     (def visited-vars (append vars (list symbol))) ;; append symbol to list of visited vars
     (if (not (member what visited-vars)) ;; check if the new symbol are in visited vars
         ;; if is not inside returns the list of vars recursively
         (append (list (cons symbol (subst expr what for))) (subst-in r-vars what for visited-vars))
         ;; if is inside returns original-vars
         original-vars
      )
    ]
   )
  )

;; subst :: Expr Symbol Expr -> Expr
;; ESCRIBE ALGO AQUI PORFA
(define (subst in what for)
  (match in
    [(real n) (real n)]
    [(imaginary i) (imaginary i)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(id x)
     (if (symbol=? x what)
         for
         (id x))]
    [(with vars body)
     (let ((subst-vars (subst-in vars what for '())))
       (with subst-vars
             (if (findf (λ (arg)
                        (match arg
                          [(cons x _)
                           (symbol=? x what)]))
                      subst-vars)
                 body
                 (subst body what for)))
      )
    ]
  )
 )
    
   
;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
;; interprents a Expr to returns a CValue (complex number).
(define (interp expr)
  (match expr
    [(real n) (compV n 0)]
    [(imaginary i) (compV 0 i)]
    [(add l r) (cmplx+ (interp l) (interp r))]
    [(sub l r) (cmplx- (interp l) (interp r))]
    [(if0 expr t-expr f-expr)
     (if (cmplx0? (interp expr))
         (interp t-expr)
         (interp f-expr)
     )
    ]
    [(with vars exprs)
     (match vars
       [(list) (interp exprs)]
       [(list var1 r-vars ...)
        (def (cons x e) var1)
        (interp (subst (with r-vars exprs) x (from-CValue (interp e)))) 
       ]
     )
    ]
    [(id x) (error 'interp "Open expression (free occurrence of ~a)" x)]
  )
 )
