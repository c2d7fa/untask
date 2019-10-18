#lang racket

(provide property
         property?
         name
         type
         get
         set
         update

         default)

;; This module builds on top of the concepts of properties defined in the "item"
;; module, by allowing for the association of certain properties to metadata,
;; such as type information and dynamic behaviour. In particular, a "property",
;; in the sense meant by this module has a name, a type, a default value and
;; functions for setting or getting the value. A function that wraps a setter is
;; said to "translate" the value, while a function that wraps the getter is said
;; to "calculate" the value.

(require (prefix-in i: "item.rkt")
         (prefix-in a: "../../attribute.rkt"))

;; A property consists of a name, a type, and the two functions calculate and
;; translate.
;;
;; The calculate function is used when looking up the property. It is given the
;; item-state and the item being looked up as arguments and must return the
;; value of that item.
;;
;; The translate function is used when setting the property. It is given the
;; item state, the item whose property is being set, and the value that the
;; property should be set to. It must return the updated item state.
;;
;; By default, calculate and translate simply get or set the property directly.
(a:define-species property (name type
                            (calculate (λ (item-state item)
                                         (i:get item-state item name)))
                            (translate (λ (item-state item value)
                                         (i:set item-state item name value)))))

;; Return the name of the given property.
(define (name property)
  (a:get-path (property property.name)))

;; Return the type of the given property.
(define (type property)
  (a:get-path (property property.type)))

;; Get the value of a property. This function is similar to item:get, but takes
;; into account the metadata associated with the give property.
(define (get item-state item property)
  ((a:get-path (property property.calculate)) item-state item))

;; Set the value of a property. This function is similar to item:set, but takes
;; into account the metadata associated with the given property.
(define (set item-state item property value)
  ((a:get-path (property property.translate)) item-state item value))

;; Update the value of a property. This function is similar to item:update, but
;; takes into account the metadata associated with the given property, both when
;; reading and writing.
(define (update item-state item property f)
  (set item-state item property (f (get item-state item property))))

;; Given a property, create an equivalent property that has a default value.
(define (default base-property default-value)
  (property #:name (name base-property)
            #:type (type base-property)
            #:calculate (λ (item-state item)
                          (or (get item-state item base-property)
                              default-value))
            #:translate (λ (item-state item value)
                          (set item-state item base-property value))))
