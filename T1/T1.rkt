#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: Joel Riquelme
RUT: 20.499.444-7
|#

;; Parte a)

#|
<CFraction> ::= (simple <value>)
              | (compound <value> <value> <CFraction>)

<value> ::= Integer

Type CFraction represent a finit Continued Fraction.
|#
(deftype CFraction
  (simple a0)
  (compound a0 b0 a1))

;; Parte b)
;; eval :: CFraction -> Rational
;; Function that returns the eval CFraction in to Rational.
(define (eval cf)
  (match cf
    [(simple a0) a0]
    [(compound a0 b0 a1) (+ a0 (/ b0 (eval a1)))]))

;; Parte c)
;; degree ::  CFraction -> Integer
;; Funtion rhat returns the degree of a CFRaction.
(define (degree cf)
  (match cf
    [(simple _) 0]
    [(compound _ _ a1) (+ 1 (degree a1))]))


;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Fold over CFrations.
(define (fold-cfraction f g)
  (λ (cf)
    (match cf
      [(simple a0) (f a0)]
      [(compound a0 b0 a1) (g a0 b0 ((fold-cfraction f g) a1))])))
                   
;; Parte e)
;; eval2 :: CFraction -> Rational
;; Function that returns the eval CFraction in to Rational.
(define eval2
  (fold-cfraction identity
                  (λ (a0 b0 a1) (+ a0 (/ b0 a1)))))

;; degree2 ::  CFraction -> Integer
;; Funtion rhat returns the degree of a CFRaction.
(define degree2
  (fold-cfraction (λ (_) 0) (λ (a0 b0 a1) (+ 1 a1))))

;; Parte f)
;; mysterious-cf :: Integer -> CFraction


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer


;; mysterious-list :: Integer -> ListOf Float


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
