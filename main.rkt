#lang racket

(require
 (prefix-in prop: "./src/untask/core/property.rkt")
 (prefix-in state: "./src/untask/core/state.rkt")
 (prefix-in export: "./src/untask/core/export.rkt")
 "./src/untask/properties/builtin.rkt"

 "./src/untask/user/loop.rkt"
 (only-in "./src/untask/command/execute.rkt" execute)
 (prefix-in interpret: "./src/untask/command/interpret.rkt")

 (prefix-in a: "./src/attribute.rkt"))

(define state-box (box state:state-empty))
(when (not (zero? (vector-length (current-command-line-arguments))))
  (run-execute! state-box `(open ,(vector-ref (current-command-line-arguments) 0))))
#;(user-loop! state-box)

(define (run-inputs! . inputs)
  (for-each (λ (input)
              (printf "> ~a~n" input)
              (interpret:run! (interpret:interpret-string input) state-box))
            inputs))

(run-inputs! "add {Item 1}"
             "context add context filter #context status:active modify #context"
             "@context"
             "add {Item 2}"
             "add {Item 3}"
             "add {Item 4} depends+2"
             "list"
             "context list"
             "-@context"
             "list"
             "context remove context"
             "context list")
