#lang racket

;; The term 'item-data' is used to refer to the database containing all items
;; and information about them. Individual items are indexed by a key. Many
;; functions in this project take an 'item-data' and an 'item' as arguments;
;; functions that modify item properties usually do so by returning an updated
;; 'item-data'.

(provide (all-defined-out))

(require
  (prefix-in prop: "./property-type.rkt"))

(define item-data-empty
  (list*
   0       ;; Next item
   (hash)  ;; Item properties
   ))

;; Returns (values new-item-data new-item).
(define (new-item item-data)
  (define next-item (car item-data))
  (define item-properties (cadr item-data))
  (values
   (list* (add1 next-item) item-properties)
   next-item))

;; Returns new-item-data.
(define (set-property item-data item property-type value)
  (define next-item (car item-data))
  (define items-properties (cadr item-data))
  (define item-properties (hash-ref items-properties item (hash)))
  (list* next-item
         (hash-set items-properties
                   item
                   (hash-set item-properties
                             (prop:property-type-key property-type)
                             value))))

(define (set-property-by-key item-data item key value #:property-types property-types)
  (set-property item-data item (prop:get-property-type property-types key) value))

(define (get-property-by-key item-data item key #:property-types property-types)
  (get-property item-data item (prop:get-property-type property-types key)))

;; Returns value.
(define (get-property item-data item property-type)
  (define item-properties (cadr item-data))
  (hash-ref (hash-ref item-properties item (hash))
            (prop:property-type-key property-type)
            (prop:property-type-default property-type)))

;; Returns set of all items.
(define (all-items item-data)
  (define item-properties (cadr item-data))
  (list->set (hash-keys item-properties)))

;; Returns the ID (an integer uniquely representing a reference to an
;; item) of an item in item-data.
(define (item-id item-data item)
  item)

;;;;

;; Returns new-item-data.
(define (update-property item-data item property-type f)
  (set-property item-data item property-type (f (get-property item-data item property-type))))

