#lang racket
(provide (all-defined-out)) ; Export all the definitions made in this module
(require ffi/unsafe/vm)
(require compatibility/defmacro)
(require racket/match (for-syntax racket/match))

; A helper macro to generate definitions
; It assumes that primitive names are prefixed with "(demo)", see the definition
; in 'bindings_racket.cpp' and 'bindings.inc'
(define-syntax (populate-prims  sprims)
  (define prims (syntax->datum sprims))
  (datum->syntax sprims
  `(begin
     ,@(map (lambda (i)
              (match i
                     [(list id args ret)
                      (let* ((strnm (string-append "(demo)" (symbol->string id))))
                        `(define ,id (vm-eval '(foreign-procedure ,strnm ,args ,ret))))]
                     )) (cdr prims)))))


(define default-ns (current-namespace))

(define (set-default-ns -ns-)
  (set! default-ns -ns-))

; This is a helper function for obtaining Racket variable values from C++ context
; It need to be told which namespace to use for lookup - i.e., a toplevel script
; must call (set-default-ns (current-namespace)) in order to make its definitions visible
(define (lookup-var id)
  (debug-log (string-append "lookup ... " (symbol->string id)))
  (parameterize ([current-namespace default-ns])
    (namespace-variable-value id #f (lambda () 'not-found))))

; nc/prims.scm is generated from 'bindings.inc' by 'bindings.scm'
(include "nc/prims.scm")

(define callbackptr '())
(define (register-callback x)
  (debug-log "CONTEXT PTR REGISTERED")
  (set! callbackptr x)
  #t
  )
