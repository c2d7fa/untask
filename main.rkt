#lang racket

(require
 (prefix-in state: "src/untask/core/state.rkt")

 "src/untask/user/execute.rkt"
 "src/untask/user/loop.rkt")

(define (main arguments)
  (define state-box (box state:state-empty))
  (void
   (when (not (zero? (vector-length arguments)))
     (execute! `(open ,(vector-ref arguments 0)) state-box))
   (user-loop! state-box)))

(main (current-command-line-arguments))
