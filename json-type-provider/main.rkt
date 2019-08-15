#lang typed/racket/base

(provide define-json-types
         JSNum)

(require racket/match
         json
         "read.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/list
                     racket/contract
                     racket/pretty
                     racket/set))

(: read-hash-rest : Input-Port (String Input-Port → Void) → Void)
(define (read-hash-rest i on-elem!)
  (define (read-pair!)
    (define k (read-a-string i))
    (let ([ch (skip-whitespace i)])
      (unless (eqv? ch #\:)
        (bad-input i))
      (read-byte i))
    (on-elem! k i))
  (match (skip-whitespace i)
    [(== #\} eqv?) (void (read-byte i))]
    [_
     (read-pair!)
     (let loop ()
       (match (skip-whitespace i)
         [(== #\} eqv?) (void (read-byte i))]
         [(== #\, eqv?) (read-byte i)
                        (read-pair!)
                        (loop)]
         [c (err i "error while parsing json object: ~a" c)]))]))

(begin-for-syntax
  (define user-types (make-parameter (seteq)))
  (define T-name (make-parameter #f))

  (define-syntax-class dec
    #:description "data declaration"
    (pattern [_:id _:desc]))

  (define-syntax-class desc
    #:description "type description"
    (pattern _:type)
    (pattern _:obj))

  (define-syntax-class type
    #:description "type description"
    (pattern _:base)
    (pattern ((~literal U) _:base ...)))

  (define-syntax-class base
    #:description "base type description"
    (pattern (~or (~literal JSNum) (~literal Real) (~literal Integer) (~literal Float) (~literal Boolean) (~literal String)))
    (pattern (~or #t #f))
    (pattern ((~literal quote) x) #:when (eq? (syntax-e #'x) 'null))
    (pattern _:id)
    (pattern _:lst)
    (pattern (_:pat (~literal =>) _ #:by _:expr)))

  (define-syntax-class obj
    #:description "object shape description"
    (pattern (_:fld ...))
    (pattern (_:fld ... (~or #:ignore-others #:log-warning-others #:error-others))))

  (define-syntax-class fld
    #:description "object field description"
    (pattern [fld:fldn (~literal :) d:type])
    (pattern [fld:fldn (~literal :) d:type #:default [e:expr (~literal :) _]]))

  (define-syntax-class fldn
    #:description "field naming"
    #:attributes (raw-name field-name)
    (pattern x:id
             #:attr raw-name #'x
             #:attr field-name #'x)
    (pattern (x:id y:id)
             #:attr raw-name #'x
             #:attr field-name #'y))

  (define-syntax-class lst
    #:description "list shape description"
    (pattern ((~literal Listof) _:type))
    (pattern ((~literal List) _:type ...))
    (pattern ((~literal List*) _:type ... ((~literal Listof) _:type))))

  (define-syntax-class pat
    #:description "layout description"
    (pattern ((~literal List) [_:id (~literal :) _:type] ...))
    (pattern ((~literal List*) [_:id (~literal :) _:type] ... [_:id (~literal :) ((~literal Listof) _:type)]))
    (pattern [_:id (~literal :) _:type])
    (pattern _:obj))

  (define gen-type-dec
    (syntax-parser
      [[T:id d:desc]
       (syntax-parse #'d
         [(f:fld ... _:keyword ...)
          (with-syntax ([(f* ...) (map gen-field-dec (syntax->list #'(f ...)))])
            #'(struct T (f* ...) #:transparent))]
         [t (with-syntax ([R (gen-type-def #'t)])
              #'(define-type T R))])]))

  (define gen-field-dec
    (syntax-parser
      [(L:fldn : D)
       (with-syntax ([R (gen-type-def #'D)]
                     [X (attribute L.field-name)])
         #'[X : R])]
      [(L:fldn _ D #:default [_ _ T])
       (with-syntax ([R (gen-type-def #'D)]
                     [X (attribute L.field-name)])
         #'[X : (U R T)])]))

  (define (gen-type-def stx)
    (define go-type
      (syntax-parser
        [((~literal U) t ...)
         (with-syntax ([(R ...) (map go-base (syntax->list #'(t ...)))])
           #'(U R ...))]
        [t (go-base #'t)]))
    (define go-base
      (syntax-parser
        [#t #'#t]
        [#f #'#f]
        [((~literal quote) x) #:when (equal? (syntax-e #'x) 'null) #'null]
        [x:id #'x]
        [(_ (~literal =>) R #:by _) #'R]
        [((~and K (~or (~literal List) (~literal List*) (~literal Listof))) t ...)
         (with-syntax ([(R ...) (map go-type (syntax->list #'(t ...)))])
           #'(K R ...))]))
    (go-type stx))

  (define gen-parse-dec
    (syntax-parser
      [[T d]
       (with-syntax* ([read-T (format-id #'T "read-~a" (syntax-e #'T))]
                      [def-T (parameterize ([T-name (syntax-e #'T)])
                               (gen-parse-def #'T #'d))])
         #'(define read-T : (Reader T)
             def-T))]))

  (define (gen-parse-def T d)
    (syntax-parse d
      [t:type (gen-parse-def/type #'t)]
      [o:obj (gen-parse-obj T #'o)]))

  (define (gen-parse-obj T o)
    (syntax-parse o
      [(f:fld ... _:keyword ...)
       (define-values (ss xs parse-Txs Txs es) (field-list-info (syntax->list #'(f ...))))
       (with-syntax ([T T]
                     [(x ...) xs])
         #`(λ (i [default bad-input])
             (match (skip-whitespace i)
               [(== #\{ eqv?)
                (void (read-byte i))
                (let-values ([(x ...) #,(gen-parse-hash-rest o #'i ss parse-Txs Txs es)])
                  (T x ...))]
               [_ (default i)])))]))

  (define (gen-parse-hash-rest o i raw-names parse-fields field-types defaults)
    (syntax-parse o
      [(_:fld ... on-other:keyword ...)
       (with-syntax* ([i i]
                      [(s ...) raw-names]
                      [(x ...) (generate-temporaries raw-names)]
                      [(default-x ...) defaults]
                      [(Tx ...) field-types]
                      [(read-Tx ...) parse-fields])
         #`(let ([x : (U #f (Boxof Tx)) #f] ...)
             (define (on-elem! [k : String] [i : Input-Port])
               (case k
                 [(s) (set! x ((inst box-immutable Tx) (read-Tx i)))] ...
                 [else #,(syntax-parse #'(on-other ...)
                           [(#:ignore-others) #'(void)]
                           [(~or () (#:log-warning-others))
                            #`(log-warning "Attribute `~a` ignored when reading `~a`" k '#,(T-name))]
                           [(#:error-others)
                            #`(err i "Attribute `~a` not expected when reading `~a`" k '#,(T-name))])
                       (skip-json i)]))
             (read-hash-rest i on-elem!)
             (values (match x
                       [(box v) v]
                       [_ default-x]) ...)))]))

  (define (field-list-info fs)
    (for/lists (ss xs parse-Txs Txs es) ([f (in-list fs)])
      (syntax-parse f
        [(f:fldn _ d)
         (with-syntax ([s (attribute f.raw-name)])
           (values (symbol->string (syntax-e #'s))
                   (attribute f.field-name)
                   (gen-parse-def/type #'d)
                   (gen-type-def #'d)
                   #`(error '#,(format-id #f "read-~a" (T-name))
                            (format "Attribute `~a` absent" 's))))]
        [(f:fldn _ d #:default [e _ T])
         (values (symbol->string (syntax-e (attribute f.raw-name)))
                 (attribute f.field-name)
                 (gen-parse-def/type #'d)
                 (gen-type-def #'d)
                 #'(ann e T))])))

  (define (gen-parse-def/type t)

    (define go-base
      (syntax-parser
        [(~or (~literal JSNum) (~literal Real)) #'read-number]
        [(~literal Integer) #'read-integer]
        [(~literal Float) #'read-float]
        [(~literal Boolean) #'read-bool]
        [(~literal String) #'read-a-string]
        [(~or ((~literal List)) (~literal Null))
         #'read-empty-list]
        [x:id
         (if (set-member? (user-types) (syntax-e #'x))
             (format-id #'x "read-~a" (syntax-e #'x))
             (raise-syntax-error
              'define-json-types
              "The only accepted type identifiers are among {JSNum, Real, Integer, Float, Boolean, String, Null, 'null} or a declared one"
              #'x))]
        [#t #'read-true]
        [#f #'read-false]
        [((~literal quote) x) #:when (eq? (syntax-e #'x) 'null) #'read-null]
        [((~literal List) t1 t ...)
         (with-syntax ([(p1 p ...) (map gen-parse-def/type (syntax->list #'(t1 t ...)))]
                       [(T1 T ...) (map gen-type-def (syntax->list #'(t1 t ...)))])
           #`(ann (λ (i [default bad-input])
                    (match (skip-whitespace i)
                      [(== #\[ eqv?)
                       (read-byte i)
                       (begin0 (list (p1 i)
                                     (match (skip-whitespace i)
                                       [(== #\, eqv?)
                                        (read-byte i)
                                        (p i)]
                                       [_ (bad-input i)])
                                     ...)
                         (unless (eqv? (skip-whitespace i) #\])
                           (bad-input i))
                         (read-byte i))]
                      [_ (default i)]))
                  (Reader (List T1 T ...))))]
        [(~or ((~literal Listof) t)
              ((~literal List* ) ((~literal Listof) t)))
         (with-syntax ([read-t (gen-parse-def/type #'t)]
                       [T (gen-type-def #'t)])
           #'((inst read-list T) read-t))]
        [((~literal List*) t1 t2 ... ((~literal Listof) t*))
         (with-syntax ([(p1 p2 ... p*) (map gen-parse-def/type (syntax->list #'(t1 t2 ... t*)))]
                       [(T1 T2 ... T*) (map gen-type-def (syntax->list #'(t1 t2 ... t*)))])
           #`(ann (λ (i [default bad-input])
                    (match (skip-whitespace i)
                      [(== #\[ eqv?)
                       (read-byte i)
                       (list* (p1 i)
                              (match (skip-whitespace i)
                                [(== #\, eqv?)
                                 (read-byte i)
                                 (p2 i)]
                                [_ (bad-input i)])
                              ...
                              (match (skip-whitespace i)
                                [(== #\, eqv?)
                                 (read-byte i)
                                 (((inst read-list-rest T*) p*) i)]
                                [(== #\] eqv?)
                                 (read-byte i)
                                 '()]
                                [_ (bad-input i)]))]
                      [_ (default i)]))
                  (Reader (List* T1 T2 ... (Listof T*)))))]
        [(p (~literal =>) T #:by e) (gen-parse-conv #'T #'p #'e)]))
    
    (syntax-parse t
      [((~literal U) b ... bₙ)
       #`(ann (λ (i [default bad-input])
                #,(foldr (λ (bᵢ acc)
                           #`(#,(gen-parse-def/type bᵢ) i (λ ([_ : Input-Port]) #,acc)))
                         #`(#,(gen-parse-def/type #'bₙ) i default)
                         (syntax->list #'(b ...))))
              (Reader #,t))]
      [t:base (go-base #'t)]))

  (define (gen-parse-conv T p e)
    (syntax-parse p
      [[x (~literal :) t]
       (with-syntax ([read-t (gen-parse-def/type #'t)])
         #`(ann (λ (i [default bad-input])
                  (let ([x (read-t i default)])
                    #,e))
                (Reader #,T)))]
      [(~and t ((~literal List) [x _ Tx] ...))
       (with-syntax ([read-t (gen-parse-def/type #'(List Tx ...))])
         #`(ann (λ (i [default bad-input])
                  (match (skip-whitespace i)
                    [(== #\[ eqv?)
                     (match-let ([(list x ...) (read-t i)])
                       #,e)]
                    [_ (default i)]))
                (Reader #,T)))]
      [(~and t ((~literal List*) [x _ Tx] ... [xs _ ((~literal Listof) T*)]))
       (with-syntax ([read-t (gen-parse-def/type #'(List* Tx ... (Listof T*)))])
         #`(ann (λ (i [default bad-input])
                  (match (skip-whitespace i)
                    [(== #\[ eqv?)
                     (match-let ([(list* x ... xs) (read-t i)])
                       #,e)]
                    [_ (default i)]))
                (Reader #,T)))]
      [(~and o (f:fld ... _:keyword ...))
       (define-values (ss xs parse-Txs Txs es) (field-list-info (syntax->list #'(f ...))))
       (with-syntax ([(x ...) xs])
         #`(ann (λ (i [default bad-input])
                  (match (skip-whitespace i)
                    [(== #\{ eqv?)
                     (read-byte i)
                     (let-values ([(x ...) #,(gen-parse-hash-rest #'o #'i ss parse-Txs Txs es)])
                       #,e)]
                    [_ (default i)]))
                (Reader #,T)))]))
  )

(define-syntax define-json-types
  (syntax-parser
    [(_ d:dec ...)
     (define ds (syntax->list #'(d ...)))
     (parameterize ([user-types (for/seteq ([d (in-list (syntax->list #'(d ...)))])
                                  (syntax-parse d
                                    [[x _] (syntax-e #'x)]))])
       (with-syntax ([(ts ...) (map gen-type-dec ds)]
                     [(ps ...) (map gen-parse-dec ds)])
         (define stx #'(begin ts ... ps ...))
         ;(pretty-print (syntax->datum stx))
         stx))]))
