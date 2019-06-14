#lang racket

(require
 (prefix-in prop: "./src/untask/core/property.rkt")
 (prefix-in state: "./src/untask/core/state.rkt")
 (prefix-in export: "./src/untask/core/export.rkt")

 "./src/untask/user/loop.rkt"
 (only-in "./src/untask/command/execute.rkt" execute)

 (prefix-in status: "./src/untask/properties/status.rkt")
 (prefix-in description: "./src/untask/properties/description.rkt")
 (prefix-in tags: "./src/untask/properties/tags.rkt")
 (prefix-in urgency: "./src/untask/properties/urgency.rkt")
 (prefix-in depends: "./src/untask/properties/dependencies.rkt")

 (only-in "./src/misc.rkt" thread-first)
 (prefix-in a: "./src/attribute.rkt"))

(define property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    (prop:add-property-type depends:blocks-property-type)
    ))

(define state-box (box state:state-empty))
(when (not (zero? (vector-length (current-command-line-arguments))))
  (run-execute! state-box `(open ,(vector-ref (current-command-line-arguments) 0)) #:property-types property-types))
(user-loop! state-box #:property-types property-types)
