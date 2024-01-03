#lang racket

(module reader syntax/module-reader
  lsl-plus)

(require (except-in lsl define let lambda))

(provide (all-from-out lsl))
(provide write define let lambda)
