#lang racket
(require peg)
(require compatibility/defmacro)
(require "infix_compiler.rkt" (for-syntax racket/match "infix_compiler.rkt"))
(provide infix-string i#)

(define-macro (i# . args)
  (if (and (null? (cdr args)) (string? (car args)))
      `(infix-string ,@args)
      `(infix-string ,(any->string args))))

(define-macro (infix-string str)
  (compile-infix-string str))

