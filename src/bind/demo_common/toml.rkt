#lang racket
(require "toml_compiler.rkt" (for-syntax racket/match "toml_compiler.rkt"))
(provide current-dirname include-toml-abs include-toml define-toml)

(define-syntax (include-toml fn)
  (define path (cadr (syntax->datum fn)))
  (datum->syntax fn
                 `(include-toml-abs ,(string-append (getenv "RKT_BIND_CONFIG_PATH") "/" path))))

(define-syntax (include-toml-abs fn)
  (define path (cadr (syntax->datum fn)))
  (datum->syntax fn (compile-toml-file path)))


; include-toml-abs now would use define-toml instead of define.
; This way we can customise what we really want to do with the TOML variable definitions.
; The default behaviour here is to both define a Racket variable and declare its name and value
; using a primitive function set-toml! - it's up to C++ bindings layer what to do with this data.
; Keep in mind that complex values (lists, vectors, etc.) cannot be stored in C++ memory as is,
; GC can move them willy-nilly any moment and they'll become invalid. C++ layer must interpret
; the data immediately before returning from set-toml! call and convert it into its own representation.
(define-syntax (define-toml x)
  (define xx (cdr (syntax->datum x)))
  (match xx
      [(list id dst)
       (datum->syntax x
        `(begin
           (define ,id ,dst)
           (set-toml! ,(symbol->string id) ,id)))]))
