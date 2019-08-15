[![Build Status](https://travis-ci.com/philnguyen/json-type-provider.svg?branch=master)](https://travis-ci.com/philnguyen/json-type-provider)
json-type-provider
=========================================

This is a library for parsing JSON input into Typed Racket values.
By specifying the JSON schema in Typed Racket, the programmer gets a well-typed parser.

Nice things the library provides:
- No error-handling boilerplate.
- Flexibility in mapping from JSON format into Racket types. For example, by default, objects are parsed into compact structs that only store fields the programmer cares about. It is also possible to specify a custom conversion that say, convert an object with `real` and `imag` directly into Racket's `Complex`.

Examples
=========================================

Examples are under [`test/`](https://github.com/philnguyen/json-type-provider/tree/master/json-type-provider/test) directory

* [london_weather.rkt](https://github.com/philnguyen/json-type-provider/blob/master/json-type-provider/test/london_weather.rkt) shows a straightforward declaration
* [complex.rkt](https://github.com/philnguyen/json-type-provider/blob/master/json-type-provider/test/complex.rkt) shows how to convert objects or lists into custom types
* [cards.rkt](https://github.com/philnguyen/json-type-provider/blob/master/json-type-provider/test/cards.rkt) shows how to impose a more precise type (e.g. enumerations) on raw data


Installation
=========================================

```
raco pkg install json-type-provider
```

Documentation
=========================================

(There will eventually be Scriblings)

The `define-json-types` macro will define a type `id` and generate a parser `read-id`
for each declared `id` type, whose RHS describes the shape of the data
and optionally how it is mapped into a custom Racket type.
```racket
(define-json-types
  [id type-desc] ...)
```

Below is the full grammar:

```racket
desc ::= obj-desc | type-desc

type-desc ::= simp-type-desc | (U simp-type-desc ...)

simp-type-desc ::= JSNum
                |  Real
                |  Integer
                |  Float
                |  Boolean
                |  String
                |  #t
                |  #f
                |  'null
                |  id
                |  list-type-desc
                |  (pat => type #:by expr)

list-type-desc ::= (List type-desc ...)
                |  (Listof type-desc)
                |  (List* type-desc ... (Listof type-desc))

pat ::= (List [id : type-desc] ...)
     |  (List* [id : type-desc] ... [id : (Listof type-desc)])
     |  obj-desc
     |  [x : type-desc]

obj-desc ::= (field-desc ... rest-option)

rest-option ::=
             | #:ignore-others
             | #:log-warning-others
             | #:error-others

field-desc ::= [field-name : type-desc]
            |  [field-name : type-desc #:default [expr : type]]


field-name ::= id
            |  (id id)

type = arbitrary Racket type
```
