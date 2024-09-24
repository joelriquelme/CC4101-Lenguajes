#lang play
(require "T2.rkt")

(print-only-errors #t)

(test (parse-prop '(not true)) (p-not (tt)))