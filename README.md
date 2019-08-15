[![Build Status](https://travis-ci.org/philnguyen/json-type-provider.svg?branch=master)](https://travis-ci.org/philnguyen/json-type-provider)
json-type-provider
=========================================

This is a library for parsing JSON input into Typed Racket values.
By specifying the JSON schema in Typed Racket, the programmer gets a well-typed parser.

Nice things the library provides:
- No error-handling boilerplate.
- Flexibility in mapping from JSON format into Racket types. For example, by default, objects are parsed into compact structs that only store fields the programmer cares about. It is also possible to specify a custom conversion that say, convert an object with `real` and `imag` directly into Racket's `Complex`.

Documentation
=========================================

(There will eventually be Scriblings)

```racket
(define-json-types
  [id type-desc] ...)

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
                |  pat => type #:by expr

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

Examples
=========================================

Examples are under [`test/`](https://github.com/philnguyen/json-type-provider/tree/master/json-type-provider/test) directory
