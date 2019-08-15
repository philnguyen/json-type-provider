#lang typed/racket/base

(require typed/rackunit
         "../main.rkt")

(define-json-types
  [Comp (([real : Float] [imag : Float])
         => Complex #:by (+ real (* imag 0+1i)))]
  [Comps (Listof Comp)]
  ;; List `Comps`, but defined inline
  [Comp-List (Listof (([real : Float] [imag : Float])
                      => Complex
                      #:by (+ real (* imag 0+1i))))])

(define res (list 1.0+2.0i 3.0+4.0i 6.0+5.0i))
(check-equal? (read-Comps     (open-input-file "complex.json")) res)
(check-equal? (read-Comp-List (open-input-file "complex.json")) res)
