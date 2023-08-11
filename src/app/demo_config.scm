(require racket/flonum (for-syntax racket/flonum))
(require ffi/unsafe/vm)
(require compatibility/defmacro)
(require racket/match (for-syntax racket/match))
(require demo_common demo_common/toml demo_common/infix
         (for-syntax demo_common))


(define ns (current-namespace))
;; lookup-var is a statically compiled funcion and won't be moved by GC,
;; it's safe to save it in C++ context
(set_lookup lookup-var)
(set-default-ns ns)

(define-syntax-rule  (set-toml! k v) (displayln (list k v)))
(include-toml-abs "pid_config.toml")

(define prev_error 0.0)
(define prev_tick 0)
(define prev_integral 0.0)
(define tick 0)

(define (scripted-update)
  ;; Execute a simple PID controller
  (set! tick (rkt-get-ntick callbackptr))
  (if (> tick prev_tick)
      (let* ((pv (rkt-get-control-var callbackptr))
             (dt (i#  base_dt * (tick - prev_tick)))
             (error (i# setpoint-pv))
             (pout (i# Kp * error))
             (integral (i# prev_integral + error * dt))
             (iout (i# Ki * integral))
             (derivative (i# (error - prev_error)/dt))
             (dout (i# Kd * derivative))
             (output (i# pout + iout + dout))
             (set_output
              
              (if (> output max_output)
                  max_output
                  (if (< output min_output)
                      min_output
                      output))))
        (set! prev_tick tick)
        (set! prev_integral integral)
        (set! prev_error error)
        (rkt-set-control-parameter callbackptr set_output))
      #t
      ))

