#lang typed/racket/base

(require racket/match
         typed/rackunit
         "../main.rkt")

(define-json-types
  [Comp (([real : Float] [imag : Float])
         => Complex #:by (+ real (* imag 0+1i)))]
  [Comps (Listof Comp)]
  ;; List `Comps`, but defined inline
  [Comp-List (Listof (([real : Float] [imag : Float])
                      => Complex
                      #:by (+ real (* imag 0+1i))))]
  ;; Read as coordinates and their summaries
  [Coord ([(real x) : Float] [(imag y) : Float])]

  ;; Example of folding without building intermediate list
  [Summary ((Listof Coord) => (Pairof Float Float)
                           #:by-folding (λ (coord acc)
                                          (match-let ([(Coord x y) coord]
                                                      [(cons sx sy) acc])
                                            (cons (+ x sx) (+ y sy))))
                           #:from (cons 0.0 0.0))])

(define res (list 1.0+2.0i 3.0+4.0i 6.0+5.0i))
(check-equal? (read-Comps     (open-input-file "complex.json")) res)
(check-equal? (read-Comp-List (open-input-file "complex.json")) res)
(check-equal? (read-Summary   (open-input-file "complex.json")) (cons 10.0 11.0))
