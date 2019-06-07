#lang racket

(require
 (prefix-in prop: "./src/untask/core/property.rkt")
 (prefix-in state: "./src/untask/core/state.rkt")

 (only-in "./src/untask/user/loop.rkt" user-loop!)

 (prefix-in status: "./src/untask/properties/status.rkt")
 (prefix-in description: "./src/untask/properties/description.rkt")
 (prefix-in tags: "./src/untask/properties/tags.rkt")
 (prefix-in urgency: "./src/untask/properties/urgency.rkt")
 (prefix-in depends: "./src/untask/properties/dependencies.rkt")

 (only-in "./src/misc.rkt" thread-first))

(define property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    (prop:add-property-type depends:blocks-property-type)
    ))

(user-loop! (box state:state-empty)
            #:property-types property-types)
