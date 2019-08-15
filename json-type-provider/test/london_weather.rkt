#lang typed/racket/base

(require typed/rackunit
         "../main.rkt")

(define-json-types
  [Response ([coord : Coord]
             [weather : (Listof Weather)]
             [(sys system) : System] ; rename field
             [name : String])]
  [Coord ([lon : JSNum] [lat : JSNum])]
  [Weather ([id : JSNum]
            [description : String])] ; ignore some fields
  [System ([type : JSNum]
           [id : JSNum]
           [message : (U JSNum String)]
           [country : String])])

(check-equal?
 (read-Response (open-input-file "london_weather.json"))
 (Response (Coord -0.13 51.51)
           (list (Weather 300 "light intensity drizzle"))
           (System 1 5091 0.0103 "GB")
           "London"))
