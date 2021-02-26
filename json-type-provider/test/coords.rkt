#lang typed/racket/base

(require typed/rackunit
         racket/match
         "../main.rkt")

(define-json-types
  [Coord ([x : Float] [y : Float] [z : Float])]
  [Coord-Sum ((Listof Coord) => (List Float Float Float Integer)
                             #:by-folding
                             (match-lambda**
                              [((Coord x y z) (list sx sy sz len))
                               (list (+ x sx) (+ y sy) (+ z sz) (+ 1 len))])
                             #:from (list 0.0 0.0 0.0 0))]
  [Message ([(coordinates coordinate-sum) : Coord-Sum])])

(match-define (Message (list x y z len)) (read-Message (open-input-file "coords.json")))
(check-equal? len 3)

