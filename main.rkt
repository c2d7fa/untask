#lang racket

(require
 (prefix-in item: "src/data/item-data.rkt")
 (prefix-in prop: "src/data/property-type.rkt")

 (only-in "src/util.rkt" thread thread-first)

 "src/user/execute.rkt"
 "src/user/loop.rkt"
 "src/user/listing.rkt"
 "src/data/export.rkt"

 (prefix-in status: "src/properties/status.rkt")
 (prefix-in description: "src/properties/description.rkt")
 (prefix-in tags: "src/properties/tags.rkt")
 (prefix-in urgency: "src/properties/urgency.rkt")
 (only-in "src/user/parser.rkt" parse)
 )

;;; EXAMPLE

(define property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:base-status-property-type)
    ;(prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:base-urgency-property-type)
    ;(prop:add-property-type urgency:urgency-property-type)
    ))

(user-loop! (box (read-item-data-from-file "./example.twd"))
            #:property-types property-types
            #:parse parse
            #:render-listing render-listing)
