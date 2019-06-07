#lang racket

;; The term 'item-data' is used to refer to the database containing all items
;; and information about them. Individual items are indexed by a key. Many
;; functions in this project take an 'item-data' and an 'item' as arguments;
;; functions that modify item properties usually do so by returning an updated
;; 'item-data'.

(provide (all-defined-out))

(require
 (prefix-in prop: "./property.rkt")
 (prefix-in a: "../../attribute.rkt"))

(a:define-record item-data (next properties))

(define item-data-empty
  (item-data #:next 0
             #:properties (hash)))

;; Returns (values new-item-data new-item).
(define (new-item item-data)
  (let ((next (add1 (a:get (item-data item-data.next)))))
    (values (a:set (item-data item-data.next) next)
            next)))

;; Returns new-item-data.
(define (set-raw-property item-data item property-type value)
  (a:set (item-data
          item-data.properties
          (a:hash.key item #:default (hash))
          (a:hash.key (prop:property-type-key property-type)))
         value))

;; Returns value.
(define (get-raw-property item-data item property-type)
  (a:get (item-data
          item-data.properties
          (a:hash.key item #:default (hash))
          (a:hash.key (prop:property-type-key property-type)
                      #:default (prop:property-type-default property-type)))))

(define (get-property item-data item property-type)
  (if (prop:property-type-calculate property-type)
      ((prop:property-type-calculate property-type) item-data item)
      (get-raw-property item-data item property-type)))

(define (set-property item-data item property-type value)
  (if (prop:property-type-translate property-type)
      ((prop:property-type-translate property-type) item-data item value)
      (set-raw-property item-data item property-type value)))

(define (set-property-by-key item-data item key value #:property-types property-types)
  (set-property item-data item (prop:get-property-type property-types key) value))

(define (get-property-by-key item-data item key #:property-types property-types)
  (get-property item-data item (prop:get-property-type property-types key)))

;; Returns set of all items.
(define (all-items item-data)
  (list->set (hash-keys (a:get (item-data item-data.properties)))))

;; Returns the ID (an integer uniquely representing a reference to an
;; item) of an item in item-data.
(define (item-id item-data item)
  item)

;;;;

;; Returns new-item-data.
(define (update-property item-data item property-type f)
  (set-property item-data item property-type (f (get-property item-data item property-type))))

