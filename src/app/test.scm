(require racket/flonum (for-syntax racket/flonum))
(require ffi/unsafe/vm)
(require compatibility/defmacro)
(require racket/match (for-syntax racket/match))
(require demo_common demo_common/toml demo_common/infix)

(define-syntax-rule  (set-toml! k v) (displayln (list k v)))

;; Default values
(include-toml-abs "test.toml")

(displayln `(VERSION: ,(get-rkt-bind-version)))
(displayln double_var)
(displayln (let ((x 2)) (infix-string "2+x*2")))
(displayln (flonum? (i# 2 + 2 * 2)))
