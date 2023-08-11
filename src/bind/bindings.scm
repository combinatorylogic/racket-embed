#lang racket

;; This is a small script that compiles bindings.inc into Scheme.
(require peg)
(require racket/match)
(require racket/pretty)

;; bindings.inc syntax definitions
(require "bindings-syntax.scm")

;; Convert C++ types to Racket core type names
(define (mktype tp)
  (match tp
         (`double 'double-float)
         (`float  'single-float)
         (`int    'integer-32)
         (else tp)))
                  

;; Translate a function definition into a list (name (arg-types...) return-type)
(define (compile-fundef sid id rettyp args)
  `(,(string->symbol sid)
    ,(map (lambda (i) (match i [(list `arg a b) (mktype (string->symbol (cdr a)))])) args) ,(mktype (string->symbol rettyp))))

;; Translate a list of function definitions
(define (parse-fundefs s)
  (let* ((ptree (peg fundefs s)))
    `(populate-prims
      ,@(map (lambda (fd)
               (match fd
                      [(list `fundef sid id rettyp args ...)
                       (compile-fundef (cdr sid) (cdr id) (cdr rettyp) args)]
                      )) (cdr ptree)))))

(define path (vector-ref (current-command-line-arguments) 0))
  
(pretty-print (parse-fundefs
               (foldl (lambda (a b) (string-append b "\n" a)) ""
                      (file->lines path)))
              (current-output-port)
              1
              )
