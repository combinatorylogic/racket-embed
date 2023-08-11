#lang racket

(provide (all-defined-out))
(require racket/flonum (for-syntax racket/flonum))
(require ffi/unsafe/vm)
(require compatibility/defmacro)
(require racket/match (for-syntax racket/match))
(require "bindings.rkt")

;; Racket pattern matching does not have support for unpacking vectors and flvectors,
;; so we're implementing our own here.
(define-macro (unpack-vector vec pattern . body)
  (define vname (gensym))
  (define vlen (length pattern))
  `(let ((,vname ,vec))
     (let (
           ,@(for/list [(i vlen)
                        (p pattern)]
                       `(,p (vector-ref ,vname ,i))))
       ,@body)))


(define-macro (unpack-flvector vec pattern . body)
  (define vname (gensym))
  (define vlen (length pattern))
  `(let ((,vname ,vec))
     (let (
           ,@(for/list [(i vlen)
                      (p pattern)]
                       `(,p (flvector-ref ,vname ,i))))
       ,@body)))

;; Guile had object->string, Racket does not, so let's add some pretty formatting to it
;; while we're at it.
(define (any->string x)
  (define o (open-output-string))
  (pretty-print x o 1)
  (get-output-string o)
  )

(define (object->string x) ( any->string x))

