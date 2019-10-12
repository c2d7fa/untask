#lang racket

(provide
 get-property-by-key
 set-property-by-key
 builtin-property-types
 get-property-type-type)

(require
 (prefix-in prop: "../core/property.rkt")
 (prefix-in item: "../core/item.rkt")
 (only-in "../../misc.rkt" thread-first)

 (prefix-in status: "./status.rkt")
 (prefix-in description: "./description.rkt")
 (prefix-in tags: "./tags.rkt")
 (prefix-in urgency: "./urgency.rkt")
 (prefix-in depends: "./dependencies.rkt")
 (prefix-in links: "./links.rkt")
 (prefix-in date: "./date.rkt")

 (prefix-in a: "../../attribute.rkt")
 )

(define (set-property-by-key item-data item key value)
  (item:set-property item-data item (prop:get-property-type builtin-property-types key) value))

(define (get-property-by-key item-data item key)
  (item:get-property item-data item (prop:get-property-type builtin-property-types key)))

(define (get-property-type-type key)
  (a:get-path ((prop:get-property-type builtin-property-types key) prop:property-type.type)))

(define builtin-property-types
  (thread-first prop:empty-property-type-collection
    (prop:add-property-type status:status-property-type)
    (prop:add-property-type description:description-property-type)
    (prop:add-property-type description:notes-property-type)
    (prop:add-property-type tags:tags-property-type)
    (prop:add-property-type urgency:urgency-property-type)
    (prop:add-property-type depends:depends-property-type)
    (prop:add-property-type depends:blocks-property-type)
    (prop:add-property-type links:children-property-type)
    (prop:add-property-type links:parents-property-type)
    (prop:add-property-type date:wait-property-type)
    (prop:add-property-type date:date-property-type)
    ))
