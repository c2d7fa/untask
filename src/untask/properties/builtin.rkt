#lang racket

(provide ref has?)

(require
 (prefix-in p: untask/src/untask/core/property)

 (prefix-in status: untask/src/untask/properties/status)
 (prefix-in description: untask/src/untask/properties/description)
 (prefix-in tags: untask/src/untask/properties/tags)
 (prefix-in urgency: untask/src/untask/properties/urgency)
 (prefix-in depends: untask/src/untask/properties/dependencies)
 (prefix-in links: untask/src/untask/properties/links)
 (prefix-in date: untask/src/untask/properties/date)

 (prefix-in a: untask/src/attribute))

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
