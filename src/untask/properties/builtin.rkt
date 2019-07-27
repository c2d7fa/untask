#lang racket

(provide
 get-property-by-key
 set-property-by-key
 builtin-property-types)

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in item: "../core/item.rkt")
 (only-in "../../misc.rkt" thread-first)

 (prefix-in status: "./status.rkt")
 (prefix-in description: "./description.rkt")
 (prefix-in tags: "./tags.rkt")
 (prefix-in urgency: "./urgency.rkt")
 (prefix-in depends: "./dependencies.rkt")
 (prefix-in date: "./date.rkt")
 )


(define (set-property-by-key item-data item key value)
  (item:set-property item-data item (prop:get-property-type builtin-property-types key) value))

(define (get-property-by-key item-data item key)
  (item:get-property item-data item (prop:get-property-type builtin-property-types key)))

(define builtin-property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    (prop:add-property-type depends:blocks-property-type)
    (prop:add-property-type date:wait-property-type)
    ))
