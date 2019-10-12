#lang racket

(provide (all-defined-out))

;; The term 'item-data' is used to refer to the database containing all items
;; and information about them. Individual items are indexed by a key. Many
;; functions in this project take an 'item-data' and an 'item' as arguments;
;; functions that modify item properties usually do so by returning an updated
;; 'item-data'.

(require "./property.rkt"
         (prefix-in a: "../../attribute.rkt"))

(a:define-species item-data (next properties))

(define item-data-empty
  (item-data #:next 0
             #:properties (hash)))

;; Returns (values new-item-data new-item).
(define (new-item item-data)
  (let ((next (add1 (a:get-path (item-data item-data.next)))))
    (values (a:set-path (item-data item-data.next) next)
            next)))

;; Returns new-item-data.
(define (remove-item item-data item)
  (a:update-path (item-data item-data.properties)
                 (λ (property-values)
                   (hash-remove property-values item))))

;; Copies all the properties of from-item into to-item. Returns updated
;; item-data.
(define (copy-item item-data from-item to-item)
  (a:update-path (item-data item-data.properties)
                 (λ (props)
                   (a:set-path (props (a:hash. to-item))
                               (a:get-path (props (a:hash. from-item #:default (hash))))))))

;; Returns new-item-data.
(define (set-raw-property item-data item property-type value)
  (a:set-path (item-data
               item-data.properties
               (a:hash. item)
               (a:hash. (a:get-path (property-type property-type.key))))
              value))

;; Returns value.
(define (get-raw-property item-data item property-type)
  (a:get-path (item-data
               item-data.properties
               (a:hash. item)
               (a:hash. (a:get-path (property-type property-type.key))
                        #:default (a:get-path (property-type property-type.default))))))

(define (get-property item-data item property-type)
  (let ((calculate (a:get-path (property-type property-type.calculate))))
    (if calculate
        (calculate item-data item)
        (get-raw-property item-data item property-type))))

(define (set-property item-data item property-type value)
  (let ((translate (a:get-path (property-type property-type.translate))))
    (if translate
        (translate item-data item value)
        (set-raw-property item-data item property-type value))))

;; Returns set of all items.
(define (all-items item-data)
  (list->set (hash-keys (a:get-path (item-data item-data.properties)))))

;; Returns the ID (an integer uniquely representing a reference to an
;; item) of an item in item-data.
(define (item-id item-data item)
  item)

;;;;

;; Returns new-item-data.
(define (update-property item-data item property-type f)
  (set-property item-data item property-type (f (get-property item-data item property-type))))

