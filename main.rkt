#lang racket

(require
 (prefix-in state: untask/src/untask/core/state)

 untask/src/untask/user/execute
 untask/src/untask/user/loop)

(define state-box (box state:state-empty))

(void
 (when (not (zero? (vector-length (current-command-line-arguments))))
   (execute! `(open ,(vector-ref (current-command-line-arguments) 0)) state-box))
 (user-loop! state-box))
