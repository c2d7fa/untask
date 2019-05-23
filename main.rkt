#lang racket

(require
 (prefix-in item: "src/data/item-data.rkt")
 (prefix-in prop: "src/data/property-type.rkt")

 (only-in "src/util.rkt" thread-first)
 (only-in "src/user/loop.rkt" user-loop!)
 (only-in "src/user/listing.rkt" render-listing)
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
    (prop:add-property-type status:base-status-property-type)
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:base-urgency-property-type)
    ;(prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    ))

(user-loop! (box (read-item-data-from-file "./example.twd"))
            #:property-types property-types
            #:parse parse
            #:render-listing render-listing)
