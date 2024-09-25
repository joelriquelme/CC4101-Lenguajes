#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

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
<expr> ::= ...
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | ...
|#
(deftype Expr
  ; ...
  (add l r)
  (sub l r)
  (if0 c t f)
  ; ...
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | ...
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) '???)

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
