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

;;; EXAMPLE:

(define d0 item-data-empty)
(define-values (d1 _1) (new-property d0 #:key 'description #:name "Description" #:default "(No description.)"))
(define-values (d2 _2) (new-property d1 #:key 'base-urgency #:name "Base Urgency" #:default 0))
(define-values (d3 i1) (new-item d2))
(define-values (d4 i2) (new-item d3))
(define d5 (set-property d4 i1 'description "Item 1"))
(define d6 (set-property d5 i1 'base-urgency 1))
(define d7 (set-property d6 i2 'base-urgency -2))

(values
 (get-property d7 i1 'description)
 (get-property d7 i1 'base-urgency)
 (get-property d7 i2 'description)
 (get-property d7 i2 'base-urgency))

