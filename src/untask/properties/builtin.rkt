#lang racket

(provide ref has? get set clone)

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in i: untask/src/untask/core/item)

 (prefix-in status: untask/src/untask/properties/status)
 (prefix-in description: untask/src/untask/properties/description)
 (prefix-in tags: untask/src/untask/properties/tags)
 (prefix-in urgency: untask/src/untask/properties/urgency)
 (prefix-in depends: untask/src/untask/properties/dependencies)
 (prefix-in links: untask/src/untask/properties/links)
 (prefix-in date: untask/src/untask/properties/date)
 (prefix-in color: untask/src/untask/properties/color)
 (prefix-in effort: untask/src/untask/properties/effort)
 (prefix-in order: untask/src/untask/properties/order)

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
        date:date-property
        date:wait-property
        color:color-property
        effort:effort-property
        order:order-property))

(define builtin-properties-hash
  (make-immutable-hash (map (λ (property)
                              (cons (p:name property) property))
                            builtin-properties)))

(define (ref name)
  (hash-ref builtin-properties-hash name))

(define (has? name)
  (hash-has-key? builtin-properties-hash name))

(define (set item-state item property-name value)
  (when (not (has? property-name))
    (error "Can't set unknown property ~A." property-name))
  (p:set item-state item (ref property-name) value))

(define (get item-state item property-name)
  (when (not (has? property-name))
    (error "Can't get unknown property ~A." property-name))
  (p:get item-state item (ref property-name)))

(define cloned-properties '(status description notes tags urgency depends blocks children parents date wait color effort order))

(define (clone item-state source)
  (let-values (((item-state* destination*) (i:new item-state)))
    (values (foldl (λ (property-name item-state)
                     (set item-state destination* property-name (get item-state source property-name)))
                   item-state*
                   cloned-properties)
            destination*)))
