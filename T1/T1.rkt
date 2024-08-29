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
(define (mysterious-cf n)
  (define (aux n o)
          (if (zero? n)
              o
              (aux (- n 1) (compound 6 (sqr (- (* 2 n) 1)) o))))
    
  (aux n (simple 6)))

;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Returns a ListOf Integers from a number to another number, e.g. (from-to 0 3) -> '(0 1 2)
(define (from-to n m)
  (define (aux-list n m l)
    (if (>= n m)
        l
        (aux-list (+ n 1) m (append l (list n)))))

  (aux-list n m (list)))

;; mysterious-list :: Integer -> ListOf Float
;; Returns a ListOf Float with eval of i-th element of (mysterious-cf i ) minus 3.
(define (mysterious-list n)
  (define l (from-to 0 n))
  (define l1 (map fl (map (λ (n) (- n 3)) (map eval2 (map mysterious-cf l)))))

  l1)


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; Trying whith k = 300 its posible to see that the number of this limit is Pi.


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Returns the CFraction representation of a Rational number.
(define (rac-to-cf n)
  
  (define (aux-rac r l)
    (define i (floor r))
    (define l1 (append l (list i)))
    (define f (- r i))
    (if (zero? f)
        l1
        (aux-rac (/ 1 f) l1)))

  (define (list-to-cf l)
    (if (= 1 (length l))
        (simple (car l))
        (compound (car l) 1 (list-to-cf (cdr l)))))

  (list-to-cf (aux-rac n (list))))
