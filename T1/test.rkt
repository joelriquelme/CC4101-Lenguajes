#lang play
(require math/flonum)
(require "T1.rkt")

(print-only-errors #t)

;; Part a)

;; simple constructor
(define a (simple 1))

(test (simple? a) #t)
(test (CFraction? a) #t)

;; compound constructor
(define cf (compound 12 1 (simple 4)))
(define cf1 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))

(test (compound? cf) #t)
(test (CFraction? cf) #t)

(test (compound? cf1) #t)
(test (CFraction? cf1) #t)


;; Part b)

;; simple eval
(define cf2 (simple 3))
(test (eval cf2) 3)

;; compound eval (int result)
(define cf3 (compound 3 2 (simple 2)))
(test (eval cf3) 4)

;; compound eval (double result)
(define cf4 (compound 1 1 (simple 2)))
(test (eval cf4) 1.5)

;; more complex case (int result)
(define cf5 (compound 3 2 (compound 1 2 (simple 2))))
(test (eval cf5) 4)

;; more complex case (double result)
(define cf6 (compound 1 1 (compound 0 2 (simple 1))))
(test (eval cf6) 1.5)


;; Part c)

;; simple degree
(define cf7 (simple 3))
(test (degree cf7) 0)

;; compound degree
(define cf8 (compound 3 2 (simple 2)))
(test (degree cf8) 1)

;; more complex case
(define cf9 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (degree cf9) 3)


;; Part d)

;; eval
(test ((fold-cfraction identity
                  (λ (a0 b0 a1) (+ a0 (/ b0 a1)))) cf3)
      4)

(test ((fold-cfraction identity
                  (λ (a0 b0 a1) (+ a0 (/ b0 a1)))) cf6)
        1.5)

;; degree
(test ((fold-cfraction (λ (_) 0) (λ (a0 b0 a1) (+ 1 a1))) cf7) 0)

(test ((fold-cfraction (λ (_) 0) (λ (a0 b0 a1) (+ 1 a1))) cf9) 3)


;; Part e)

;; simple eval
(define cf10 (simple 3))
(test (eval2 cf10) 3)

;; compound eval (int result)
(define cf11 (compound 3 2 (simple 2)))
(test (eval2 cf11) 4)

;; compound eval (double result)
(define cf12 (compound 1 1 (simple 2)))
(test (eval2 cf12) 1.5)

;; more complex case (int result)
(define cf13 (compound 3 2 (compound 1 2 (simple 2))))
(test (eval2 cf13) 4)

;; more complex case (double result)
(define cf14 (compound 1 1 (compound 0 2 (simple 1))))
(test (eval2 cf14) 1.5)


;; simple degree
(define cf15 (simple 3))
(test (degree2 cf15) 0)

;; compound degree
(define cf16 (compound 3 2 (simple 2)))
(test (degree2 cf16) 1)

;; more complex case
(define cf17 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (degree2 cf17) 3)


;; Part f)

(define cf18 (mysterious-cf 0))
(test cf18 (simple 6))

(define cf19 (mysterious-cf 1))
(test cf19 (compound 6 (sqr 1)(simple 6)))

(define cf20 (mysterious-cf 3))
(test cf20 (compound 6 (sqr 1)(compound 6 (sqr 3) (compound 6 (sqr 5) (simple 6)))))


;; Part g)

;; Test from-to
(define l (from-to 0 0))
(test l '())

(define l1 (from-to 0 3))
(test l1 '(0 1 2))

;; Test mysterious-list
(define l2 (mysterious-list 1))
(test l2 (list
          (fl (- (eval (mysterious-cf 0)) 3))))

(define l3 (mysterious-list 2))
(test l3 (list
          (fl (- (eval (mysterious-cf 0)) 3))
          (fl (- (eval (mysterious-cf 1)) 3))))

(define l4 (mysterious-list 3))
(test l4 (list
          (fl (- (eval (mysterious-cf 0)) 3))
          (fl (- (eval (mysterious-cf 1)) 3))
          (fl (- (eval (mysterious-cf 2)) 3))))

(define l5 (mysterious-list 0))
(test l5 '())


;; Part h)

(define l6 (rac-to-cf (+ 3 49/200)))
(test l6 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))

(define l7 (rac-to-cf 0))
(test l7 (simple 0))

