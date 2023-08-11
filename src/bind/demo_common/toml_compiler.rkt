#lang racket
(require peg)
(require racket/match)
(require racket/pretty (for-syntax racket/match racket/pretty))
(provide compile-toml-file current-dirname)

(require "nc/toml-syntax.rkt")


(define (flatten lst)
  (match lst
     ((cons (? symbol? hd)  tl) lst)
     ((list hd) (flatten hd))
     ((cons hd tl) (cons (flatten hd) (flatten2 tl)))
     (else lst)))


(define (flatten2 lst)
  (match lst
     ((cons (? symbol? hd) tl) (list lst))
     ((list hd) (flatten2 hd))
     ((cons hd tl) (cons (flatten hd) (flatten2 tl)))
     (else lst)))

(define (compile-ident pfx v)
  (if (null? pfx) (string->symbol v)
      (string->symbol (string-append pfx "." v))))


(define (compile-value v)
  (match v
    ((cons `Xnumvalue v) (string->number v))
    ((cons `Xcornumvalue v) (string->number (string-append v "e0"))) ;; parsing all numbers as floats
    ((cons `Xstringvalue v) v)
    ((cons `Xbooltrue v) #t)
    ((cons `Xboolfalse v) #f)
    ((list `Xlistvalue h t ...) (cons (compile-value h) (compile-values t)))
    ((list `Xtablevalue h t ...) (cons (compile-pair h) (compile-pairs t)))
    (else `(quote (ERROR: ,v)))))


(define (compile-values v)
  (map compile-value (flatten v)))

(define (compile-pairs v)
  (map compile-pair (flatten v)))

(define (compile-pair v)
  (match v
     ((list `Xpair k v) `(,(compile-ident '() k) ,(compile-value v)))
     (else `(quote ()))))

(define (compile-toml pfx ast)
  (match ast
    ((cons `Xtoml elts) `(begin ,@(map (lambda (v) (compile-toml pfx v)) elts)))
    ((list `Xpair k v) `(define-toml ,(compile-ident pfx k) (quote ,(compile-value v))))
    ((list `Xgroup h rst ...) `(begin ,@(map (lambda (v) (compile-toml h v)) rst)))
    ((cons `Xgroup h) `(begin ))
    ((cons h t) `(begin ,@(map (lambda (v) (compile-toml pfx v)) ast)))
    (else `(begin ))))

(define (compile-toml-file fn)
  (define tsrc (foldl (lambda (a b) (string-append b "\n" a)) ""
                      (file->lines fn)))
  (define tast (peg Xtoml tsrc))
  (define tcomp (compile-toml '() tast))
  tcomp)

(define (current-dirname)
  (path-only (path->complete-path (find-system-path 'run-file))))
