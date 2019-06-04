#lang racket

(require
 (prefix-in item: "src/data/item-data.rkt")
 (prefix-in prop: "src/data/property-type.rkt")
 (prefix-in state: "src/data/state.rkt")

 (only-in "src/util.rkt" thread-first)
 (only-in "src/user/loop.rkt" user-loop!)
 (only-in "src/data/export.rkt" read-item-data-from-file)
 (only-in "src/user/parser.rkt" parse)

 (prefix-in status: "src/properties/status.rkt")
 (prefix-in description: "src/properties/description.rkt")
 (prefix-in tags: "src/properties/tags.rkt")
 (prefix-in urgency: "src/properties/urgency.rkt")
 (prefix-in depends: "src/properties/dependencies.rkt")
 )

;;; EXAMPLE

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
