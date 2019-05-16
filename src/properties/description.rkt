#lang racket

(provide (all-defined-out))

(require (prefix-in data: "../data/item-data.rkt"))

(define description-key 'description)

(define (register-property-description item-data)
  (data:new-property item-data #:key description-key #:name "Description" #:default "(No description)"))

(define (get-description item-data item)
  (data:get-property item-data item description-key))

(define (set-description item-data item description)
  (data:set-property item-data item description-key description))

;; Returns (values new-item-data new-item).
(define (new-item-with-description item-data description)
  (define-values (new-item-data new-item) (data:new-item item-data))
  (values
   (set-description new-item-data new-item description)
   new-item))

