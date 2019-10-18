#lang racket

(provide ref has?)

(require
 (prefix-in p: "../core/property.rkt")

 (prefix-in status: "./status.rkt")
 (prefix-in description: "./description.rkt")
 (prefix-in tags: "./tags.rkt")
 (prefix-in urgency: "./urgency.rkt")
 (prefix-in depends: "./dependencies.rkt")
 (prefix-in links: "./links.rkt")
 (prefix-in date: "./date.rkt")

 (prefix-in a: "../../attribute.rkt"))

(define builtin-properties
  (list status:status-property
        description:description-property
        description:notes-property
        tags:tags-property
        urgency:urgency-property
        depends:depends-property
        depends:blocks-property
        links:children-property
        links:parents-property
        date:wait-property
        date:date-property))

(define builtin-properties-hash
  (make-immutable-hash (map (λ (property)
                              (cons (p:name property) property))
                            builtin-properties)))

(define (ref name)
  (hash-ref builtin-properties-hash name))

(define (has? name)
  (hash-has-key? builtin-properties-hash name))
