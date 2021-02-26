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
  [Coord ([(real x) : Float] [(imag y) : Float])])

(define res (list 1.0+2.0i 3.0+4.0i 6.0+5.0i))
(check-equal? (read-Comps     (open-input-file "complex.json")) res)
(check-equal? (read-Comp-List (open-input-file "complex.json")) res)


(define read-summary
  ((inst read-fold Coord (Pairof Float Float))
   (cons 0.0 0.0)
   (match-lambda**
    [((Coord x y) (cons sx sy)) (cons (+ x sx) (+ y sy))])
   read-Coord))
(check-equal? (read-summary (open-input-file "complex.json")) (cons 10.0 11.0))

(let-values ([(sx sy)
              (for/fold ([sx : Float 0.0] [sy : Float 0.0])
                        ([coord ((make-sequence-reader read-Coord) (open-input-file "complex.json"))])
                (values (+ sx (Coord-x coord))
                        (+ sy (Coord-y coord))))])
  (check-equal? sx 10.0)
  (check-equal? sy 11.0))
