#lang racket

(provide execute!)

(require
 "../command/interpret.rkt"
 "run.rkt"
 )

;; Execute a command and update the state by setting the boxed state.
(define (execute! command state-box)
  (run! (interpret command) state-box))
