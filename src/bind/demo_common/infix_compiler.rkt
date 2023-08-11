#lang racket
(require peg)
(require racket/match)
(require racket/pretty (for-syntax racket/match racket/pretty))
(provide compile-infix compile-infix-string any->string)
(require "nc/infix-syntax.rkt")

(define (fixleft op)
  (match op
    ((cons `leftass x) (xleftass op))
    ((list `unleft x) (xleftass op))
    ((list `stopfix x) (fixleft x))
    (else op)))

(define (xleftass op)
  (match op
     ((list `unleft (cons `leftass x)) x)
     ((cons `leftass (list op1 a1 (cons `leftass (list op2 a2 b2))))
      `(leftass . (,op2 (stopfix (,op1 ,(fixleft a1) ,(fixleft a2))) ,b2)))
     ((cons `leftass x) x)
     ((list `stopfix x) (xleftass x))
     (else op)))

(define (turn-leftass op) (compile-infix (fixleft op)))

(define (compile-arglist a)
  (match a
    ((list `fnpair a b) (cons (compile-infix a) (compile-arglist b)))
    ((list `fnapair a b) (cons (compile-infix a) (compile-arglist b)))
    ((cons `fnone a) (list (compile-infix a)))
    ((cons `fnaone a) (list (compile-infix a)))
    (else `("ERROR" ,a))))

(define (compile-infix t)
  (match t
    ((list `iinfix v) (compile-infix v))
    ((cons `leftass op) (turn-leftass t))
    ((list `iadd a b) `(+ ,(compile-infix a) ,(compile-infix b)))
    ((list `isub a b) `(- ,(compile-infix a) ,(compile-infix b)))
    ((list `imul a b) `(* ,(compile-infix a) ,(compile-infix b)))
    ((list `idiv a b) `(/ ,(compile-infix a) ,(compile-infix b)))
    ((cons `ivar s) (string->symbol s))
    ((cons `Xnumvalue s) (string->number s))
    ((cons `Xcornumvalue s) (string->number (string-append s "e0")))
    ((list `stopfix v) (compile-infix v))
    ((list `ifnapp f args) `(,(compile-infix f) ,@(compile-arglist args)))
    ((list `ilet f v body) `(let ((,(compile-infix f) ,(compile-infix v)))
                              ,(compile-infix body)))
    ((list `ilambda args body) `(lambda ,(compile-arglist args) ,(compile-infix body)))
    (else `("ERROR" ,t))))


(define (compile-infix-string s)
  (define tast (peg iinfix s))
  (define tcomp (compile-infix tast))
  tcomp
  )


(define (any->string x)
  (define o (open-output-string))
  (pretty-print x o 1)
  (get-output-string o)
  )
