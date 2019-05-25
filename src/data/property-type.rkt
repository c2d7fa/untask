#lang racket

;; A property type is a way of representing a certain class of properties.
;; Examples of property types are 'urgency', 'description' and 'status'.
;; Definitions of concrete property types are defined in ../properties/. The
;; property values of each item is handled in ./item-data.rkt.

(provide (all-defined-out))

;;; Property type collection

;; Property type collections are used to access information about all the
;; property types that the program has been configured to understand.

(define empty-property-type-collection
  (hash))

(define (add-property-type collection property-type)
  (hash-set collection (property-type-key property-type) property-type))

(define (get-property-type collection key)
  (hash-ref collection key))

;;; Property type

(define (make-property-type #:key key
                            #:default (default #f)
                            #:calculate (calculate #f)
                            #:translate (translate #f))
  (list key default calculate translate))

(define (property-type-key t) (list-ref t 0))
(define (property-type-default t) (list-ref t 1))
(define (property-type-calculate t) (list-ref t 2))
(define (property-type-translate t) (list-ref t 3))