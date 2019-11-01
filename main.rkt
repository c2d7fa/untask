#lang racket

(require
 (prefix-in state: untask/src/untask/core/state)

 untask/src/untask/user/execute
 untask/src/untask/user/loop)

(define (main arguments)
  (define state-box (box state:state-empty))
  (void
   (when (not (zero? (vector-length arguments)))
     (execute! `(open ,(vector-ref arguments 0)) state-box))
   (user-loop! state-box)))

(main (current-command-line-arguments))
