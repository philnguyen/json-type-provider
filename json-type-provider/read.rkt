#lang typed/racket/base

;; Originally copied and modified from `json` package

(provide
 JSNum
 Reader
 
 read-number
 read-integer
 read-float
 read-a-string
 read-bool
 read-true
 read-false
 read-null
 read-fold
 read-list
 read-list-rest
 read-empty-list
 skip-json

 skip-whitespace
 bad-input
 err
 )

(require racket/match)

(require/typed syntax/readerr
  [raise-read-error
   (String Any (Option Natural) (Option Natural) (Option Natural) (Option Natural)
           → Nothing)])

(require/typed json
  [read-json (Input-Port → Any)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Readers for base types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type (Reader X) (∀ (Y)
                           (case->
                            [Input-Port → X]
                            [Input-Port (Input-Port → Y) → (U X Y)])))
(define-type JSNum (U Integer Float))
(define-type Sign (U -1 1))

(: read-number (Reader JSNum))
(define (read-number i [default bad-input])
  (: digit-byte? : (U EOF Byte) → Boolean : #:+ Byte)
  (define (digit-byte? c)
    (and (not (eof-object? c))
         (<= (char->integer #\0) c (char->integer #\9))))

  (define (to-number [c : Byte]) (- c (char->integer #\0)))
  (define (maybe-bytes [c : (U EOF Byte)]) (if (eof-object? c) #"" (bytes c)))

  (: n->string : Integer Integer → Bytes)
  (define (n->string n exp)
    (define s (number->string n))
    (string->bytes/utf-8
     (cond
       [(zero? exp) s]
       [else
        (define m (+ (string-length s) exp))
        (string-append (substring s 0 m) "." (substring s m))])))

  (: read-integer : Sign → JSNum)
  (define (read-integer sgn)
    (define c (read-byte i))
    (cond
      [(digit-byte? c)
       (read-integer-rest sgn (to-number c)
                          #:more-digits? (not (eqv? c (char->integer #\0))))]
      [else (bad-input i (bytes-append (if (sgn . < . 0) #"-" #"")
                                       (maybe-bytes c))
                       #:eof? (eof-object? c))]))

  (: read-integer-rest : Sign Integer #:more-digits? Boolean → JSNum)
  (define (read-integer-rest sgn n #:more-digits? more-digits?)
    (define c (peek-byte i))
    (cond
      [(and more-digits? (digit-byte? c))
       (read-byte i)
       (read-integer-rest sgn (+ (* n 10) (to-number c)) #:more-digits? #t)]
      [(eqv? c (char->integer #\.))
       (read-byte i)
       (read-fraction sgn n)]
      [(or (eqv? c (char->integer #\e))
           (eqv? c (char->integer #\E)))
       (read-byte i)
       (read-exponent (* sgn n) (assert c byte?) 0)]
      [else (* sgn n)]))

  (: read-fraction : Sign Integer → Float)
  (define (read-fraction sgn n)
    (define c (read-byte i))
    (cond
      [(digit-byte? c)
       (read-fraction-rest sgn (+ (* n 10) (to-number c)) -1)]
      [else (bad-input i (bytes-append (string->bytes/utf-8 (format "~a." (* sgn n)))
                                       (maybe-bytes c))
                       #:eof? (eof-object? c))]))

  (: read-fraction-rest : Sign Integer Integer → Float)
  (define (read-fraction-rest sgn n exp)
    (define c (peek-byte i))
    (cond
      [(digit-byte? c)
       (read-byte i)
       (read-fraction-rest sgn (+ (* n 10) (to-number c)) (sub1 exp))]
      [(or (eqv? c (char->integer #\e))
           (eqv? c (char->integer #\E)))
       (read-byte i)
       (read-exponent (* sgn n) (assert c byte?) exp)]
      [else (exact->inexact (* sgn n (expt 10 exp)))]))

  (: read-exponent : Integer Byte Integer → Float)
  (define (read-exponent n mark exp)
    (define c (read-byte i))
    (cond
      [(digit-byte? c)
       (read-exponent-rest n exp (to-number c) 1)]
      [(eqv? c (char->integer #\+))
       (read-exponent-more n mark #"+" exp 1)]
      [(eqv? c (char->integer #\-))
       (read-exponent-more n mark #"-" exp -1)]
      [else (bad-input i (bytes-append (n->string n exp)
                                       (bytes mark)
                                       (maybe-bytes c))
                       #:eof? (eof-object? c))]))

  (: read-exponent-more : Integer Byte Bytes Integer Integer → Float)
  (define (read-exponent-more n mark mark2 exp sgn)
    (define c (read-byte i))
    (cond
      [(digit-byte? c)
       (read-exponent-rest n exp (to-number c) sgn)]
      [else (bad-input i (bytes-append (n->string n exp)
                                       (bytes mark)
                                       mark2
                                       (maybe-bytes c))
                       #:eof? (eof-object? c))]))

  (: read-exponent-rest : Integer Integer Integer Integer → Float)
  (define (read-exponent-rest n exp exp2 sgn)
    (define c (peek-byte i))
    (cond
      [(digit-byte? c)
       (read-byte i)
       (read-exponent-rest n exp (+ (* 10 exp2) (to-number c)) sgn)]
      [else (exact->inexact (* n (expt 10 (+ exp (* sgn exp2)))))]))

  (let ([ch (skip-whitespace i)])
    (cond [(eof-object? ch) (bad-input i)]
          [(eqv? ch #\-) (read-byte i)
                         (read-integer -1)]
          [(<= (char->integer #\0) (char->integer ch) (char->integer #\9))
           (read-integer 1)]
          [else (default i)])))

(: read-integer : (Reader Integer))
(define (read-integer i [default bad-input])
  (define n (read-number i default))
  (cond [(exact-integer? n) n]
        [(integer? n) (assert (inexact->exact n) exact-integer?)]
        [else (err i "expect integer, got ~a" n)]))

(: read-float : (Reader Float))
(define (read-float i [default bad-input])
  (define n (read-number i default))
  (if (exact-integer? n) (exact->inexact n) n))

(: read-a-string (Reader String))
(define (read-a-string i [default bad-input])

  (define (byte-char=? [b : Byte] [ch : Char]) (eqv? b (char->integer ch)))

  (: keep-char : Char String Integer (Option Bytes-Converter) → String)
  (define (keep-char c old-result pos converter)
    (define result
      (if (= pos (string-length old-result))
          (let ([new (make-string (* pos 2))])
            (string-copy! new 0 old-result 0 pos)
            new)
          old-result))
    (string-set! result pos c)
    (loop result (add1 pos) converter))

  (: loop : String Integer (Option Bytes-Converter) → String)
  (define (loop result pos converter)
    (define c (read-byte i))
    (cond
      [(eof-object? c) (err i "unterminated string")]
      [(byte-char=? c #\") (substring result 0 pos)]
      [(byte-char=? c #\\) (read-escape (read-char i) result pos converter)]
      [(c . < . 128) (keep-char (integer->char c) result pos converter)]
      [else
       ;; need to decode, but we can't un-read the byte, and
       ;; also we want to report decoding errors
       (define cvtr (or converter ID-CONVERTER))
       (define buf (make-bytes 6 c))
       (let utf8-loop ([start : Natural 0] [end : Natural 1])
         (define-values (wrote-n read-n state) (bytes-convert cvtr buf start end buf 0 6))
         (case state
           [(complete)
            (keep-char (bytes-utf-8-ref buf 0) result pos cvtr)]
           [(error)
            (err i "UTF-8 decoding error at ~e" (subbytes buf 0 end))]
           [(aborts)
            (define c (read-byte i))
            (cond
              [(eof-object? c)
               (err i "unexpected end-of-file")]
              [else
               (bytes-set! buf end c)
               (utf8-loop (+ start read-n) (add1 end))])]
           [else (error 'utf8-loop "internal")]))]))

  (: read-escape : (U EOF Char) String Integer (Option Bytes-Converter) → String)
  (define (read-escape esc result pos converter)
    (cond
      [(case esc
         [(#\b) "\b"]
         [(#\n) "\n"]
         [(#\r) "\r"]
         [(#\f) "\f"]
         [(#\t) "\t"]
         [(#\\) "\\"]
         [(#\") "\""]
         [(#\/) "/"]
         [else #f])
       => (λ (s) (keep-char (string-ref s 0) result pos converter))]
      [(eqv? esc #\u)
       (define (get-hex)
         (define (read-next) : Byte
           (define c (read-byte i))
           (when (eof-object? c) (error "unexpected end-of-file"))
           c)
         (define c1 (read-next))
         (define c2 (read-next))
         (define c3 (read-next))
         (define c4 (read-next))
         (define (hex-convert [c : Byte])
           (cond
             [(<= (char->integer #\0) c (char->integer #\9))
              (- c (char->integer #\0))]
             [(<= (char->integer #\a) c (char->integer #\f))
              (- c (- (char->integer #\a) 10))]
             [(<= (char->integer #\A) c (char->integer #\F))
              (- c (- (char->integer #\A) 10))]
             [else (err i "bad \\u escape ~e" (bytes c1 c2 c3 c4))]))
         (+ (arithmetic-shift (hex-convert c1) 12)
            (arithmetic-shift (hex-convert c2) 8)
            (arithmetic-shift (hex-convert c3) 4)
            (hex-convert c4)))
       (define e (get-hex))
       (define e*
         (cond
           [(<= #xD800 e #xDFFF)
            (define (err-missing)
              (err i "bad string \\u escape, missing second half of a UTF-16 pair"))
            (unless (eqv? (read-byte i) (char->integer #\\)) (err-missing))
            (unless (eqv? (read-byte i) (char->integer #\u)) (err-missing))
            (define e2 (get-hex))
            (cond
              [(<= #xDC00 e2 #xDFFF)
               (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000)]
              [else
               (err i "bad string \\u escape, bad second half of a UTF-16 pair")])]
           [else e]))
       (keep-char (integer->char e*) result pos converter)]
      [else (err i "bad string escape: \"~a\"" esc)]))

  (match (skip-whitespace i)
    [(== #\" eqv?) (read-byte i)
                   (loop (make-string 16) 0 #f)]
    [_ (default i)]))

(: read-bool (Reader Boolean))
(define (read-bool i [default bad-input])
  (match (skip-whitespace i)
    [(== #\t eqv?) (consume-literal i #"true") #t]
    [(== #\f eqv?) (consume-literal i #"false") #f]
    [_ (default i)]))

(: read-const (∀ (X) (Bytes X → (Reader X))))
(define (read-const lit c)
  (define l0 (integer->char (bytes-ref lit 0)))
  (λ (i [default bad-input])
    (if (eqv? (skip-whitespace i) l0)
        (begin (consume-literal i lit) c)
        (default i))))

(define read-true (read-const #"true" #t))
(define read-false (read-const #"false" #f))
(define (read-null) (read-const #"null" 'null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Combinators for reading lists and objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: read-fold (∀ (X A) A (X A → A) (Input-Port → X) → (Reader A)))
(define ((read-fold a f read-one) i [default bad-input])
  (let ([ch (skip-whitespace i)])
    (cond [(eqv? ch #\[) (read-byte i)
                         ((read-fold-rest a f read-one) i)]
          [else (default i)])))

(: read-fold-rest (∀ (X A) A (X A → A) (Input-Port → X) → Input-Port → A))
(define ((read-fold-rest a f read-one) i)
  (define ch (skip-whitespace i))
  (cond
    [(eqv? ch #\]) (read-byte i)
                   a]
    [else
     (let loop : A ([a : A (f (read-one i) a)])
       (define ch (skip-whitespace i))
       (cond
         [(eqv? ch #\]) (read-byte i)
                        a]
         [(eqv? ch #\,) (read-byte i)
                        (loop (f (read-one i) a))]
         [else (err i "error while parsing a list")]))]))

(: read-list (∀ (A) (Input-Port → A) → (Reader (Listof A))))
(define ((read-list read-one) i [default bad-input])
  (let ([ch (skip-whitespace i)])
    (cond [(eqv? ch #\[) (read-byte i)
                         ((read-list-rest read-one) i)]
          [else (default i)])))

(: read-list-rest (∀ (A) (Input-Port → A) → Input-Port → (Listof A)))
(define ((read-list-rest read-one) i)
  (reverse (((inst read-fold-rest A (Listof A)) '() cons read-one) i)))

(: read-empty-list (Reader Null))
(define (read-empty-list i [default bad-input])
  (match (skip-whitespace i)
    [(== #\[ eqv?) (read-byte i)
                   (match (skip-whitespace i)
                     [(== #\] eqv?) (read-byte i)
                                    '()]
                     [_ (bad-input i)])]
    [_ (default i)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ID-CONVERTER (assert (bytes-open-converter "UTF-8" "UTF-8")))

(: skip-json : Input-Port → Void)
(define (skip-json i) (void (read-json i)))

(: consume-literal : Input-Port Bytes → Void)
(define (consume-literal i bstr)
  (define len (bytes-length bstr))
  (read-byte i)
  (for ([j (in-range 1 len)])
    (define c (assert (read-byte i) byte?))
    (unless (eqv? c (bytes-ref bstr j))
      (bad-input i (bytes-append (subbytes bstr 0 j) (bytes c)))))
  ;; Check for delimiter, defined for our purposes as matching #rx"\\b":
  (define b (peek-byte i))
  (unless (eof-object? b)
    (when (or (<= (char->integer #\a) b (char->integer #\z))
              (<= (char->integer #\A) b (char->integer #\Z))
              (<= (char->integer #\0) b (char->integer #\9))
              (eqv? b (char->integer #\_)))
      (bad-input i bstr))))

(: skip-whitespace : Input-Port → (U Char EOF))
(define (skip-whitespace i)
  (match (peek-char i)
    [(and (? char?) (? char-whitespace?))
     (read-char i)
     (skip-whitespace i)]
    [ch ch]))

;; Follows the specification (eg, at json.org) -- no extensions.
(: err : Input-Port String Any * → Nothing)
(define (err i fmt . args)
  (define-values (l c p) (port-next-location i))
  (raise-read-error (apply format fmt args)
                    (object-name i) l c p #f))

(: bad-input (->* [Input-Port] [Bytes #:eof? (U Bytes #t #f)] Nothing))
(define (bad-input i [prefix #""] #:eof? [eof? #f])
  (define bstr (peek-bytes (assert (sub1 (error-print-width)) index?) 0 i))
  (if (or (and (eof-object? bstr) (equal? prefix #""))
          eof?)
      (err i (string-append "unexpected end-of-file"
                            (if (equal? prefix #"")
                                ""
                                (format "after ~e" prefix))))
      (err i (format "bad input starting ~e" (bytes-append prefix (if (eof-object? bstr)
                                                                      #""
                                                                      bstr))))))
