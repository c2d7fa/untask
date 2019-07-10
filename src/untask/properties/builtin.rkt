#lang racket

(provide builtin-property-types)

(require
 (prefix-in prop: "../core/property.rkt")
 (only-in "../../misc.rkt" thread-first)

 (prefix-in status: "./status.rkt")
 (prefix-in description: "./description.rkt")
 (prefix-in tags: "./tags.rkt")
 (prefix-in urgency: "./urgency.rkt")
 (prefix-in depends: "./dependencies.rkt")
 )

(define builtin-property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    (prop:add-property-type depends:blocks-property-type)
    ))
