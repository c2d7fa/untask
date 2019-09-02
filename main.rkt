#lang racket

(require
 (prefix-in state: "./src/untask/core/state.rkt")

 "./src/untask/user/execute.rkt"
 "./src/untask/user/loop.rkt")

(define state-box (box state:state-empty))

(void
 (when (not (zero? (vector-length (current-command-line-arguments))))
   (execute! `(open ,(vector-ref (current-command-line-arguments) 0)) state-box))
 (user-loop! state-box))
