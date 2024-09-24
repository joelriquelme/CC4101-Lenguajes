#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== T2 ==== ;;
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
         | (p-where <sym> <prop> <prop>)
         | (p-id <sym>)
Prop represents a Boolean values and operators
|#

(deftype Prop
  (tt)
  (ff)
  (p-not p)
  (p-and ps)
  (p-or ps)
  (p-id x)
  (p-where x name-expr body))


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
;; Parses lenguaje of Boolean propositions
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
    [(list (? symbol? x) 'where (list named-expr sprop))
     (p-where (parse-prop named-expr) x (parse-prop sprop))]
    ))

;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ttV
          | ffV
PValue is a recursive data type that represents the notion of the values in the language
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
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id x)
     (if (equal? x name)
         substitution
         (p-id x))]
    [(p-not s)   
     (p-not (p-subst s name substitution))]  
    [(p-and sprop) 
     (p-and (map p-subst sprop name substitution))]
    [(p-or sprop) 
     (p-or (map p-subst sprop name substitution))]
    [(p-where x name-expr body)
     (p-where x name-expr (p-subst body name substitution))] 
   ))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps) '???)

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) '???)

;; p-eval : Prop -> PValue
(define (p-eval p) '???)

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
