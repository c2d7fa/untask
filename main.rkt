#lang racket

(require
 (prefix-in prop: "./src/untask/core/property.rkt")
 (prefix-in state: "./src/untask/core/state.rkt")
 (prefix-in export: "./src/untask/core/export.rkt")
 "./src/untask/properties/builtin.rkt"

 "./src/untask/user/loop.rkt"
 (prefix-in interpret: "./src/untask/command/interpret.rkt")

 (prefix-in a: "./src/attribute.rkt"))

(define state-box (box state:state-empty))

(void
 (when (not (zero? (vector-length (current-command-line-arguments))))
   (interpret:run! (interpret:interpret `(open ,(vector-ref (current-command-line-arguments) 0))) state-box)))

(user-loop! state-box)
