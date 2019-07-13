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
  (run-execute! state-box `(open ,(vector-ref (current-command-line-arguments) 0)) #:property-types builtin-property-types))
#;(user-loop! state-box #:property-types builtin-property-types)

(define (run-inputs! . inputs)
  (for-each (λ (input)
              (printf "> ~a~n" input)
              (interpret:run! (interpret:interpret-string input) state-box))
            inputs))

(run-inputs! "add {This is a new item that I just added.} #sometag urgency+$6"
             "this is a parse error!"
             "add {Another new item} #sometag"
             "add {Item without tags}"
             "#sometag modify urgency-$5"
             "list"
             "urgency<$0 remove"
             "info")
