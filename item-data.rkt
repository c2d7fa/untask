#lang racket

(provide (all-defined-out))

;; The term 'item-data' is used to refer to the database containing all items
;; and information about them. Individual items are indexed by a key. Many
;; functions in this project take an 'item-data' and an 'item' as arguments;
;; functions that modify item properties usually do so by returning an updated
;; 'item-data'.

(define item-data-empty
  (list*
   0       ;; Next item
   (hash)  ;; Item properties
   (hash)  ;; Property metadata
   ))

;; Returns (values new-item-data new-item).
(define (new-item item-data)
  (define next-item (car item-data))
  (define item-properties (cadr item-data))
  (define property-metadata (cddr item-data))
  (values
   (list* (add1 next-item) item-properties property-metadata)
   next-item))

;; Returns (values new-item-data defined-property-key)
(define (new-property item-data #:key key #:name name #:default default)
  (define next-item (car item-data))
  (define item-properties (cadr item-data))
  (define properties-metadata (cddr item-data))
  (define property (list* name default))
  (values
   (list* next-item item-properties (hash-set properties-metadata key property))
   key))

;; Returns new-item-data.
(define (set-property item-data item key value)
  (define next-item (car item-data))
  (define items-properties (cadr item-data))
  (define item-properties (hash-ref items-properties item (hash)))
  (define properties-metadata (cddr item-data))
  (list* next-item (hash-set items-properties item (hash-set item-properties key value)) properties-metadata))

;; Returns value.
(define (get-property item-data item key)
  (define item-properties (cadr item-data))
  (define properties-metadata (cddr item-data))
  (define property-metadata (hash-ref properties-metadata key))
  (define default-value (cdr property-metadata))
  (hash-ref (hash-ref item-properties item (hash)) key default-value))

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
(define (update-property item-data item key f)
  (set-property item-data item key (f (get-property item-data item key))))

