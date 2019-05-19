#lang racket

(provide (all-defined-out))

(require (prefix-in data: "../data/item-data.rkt")
         (prefix-in val: "../data/values.rkt"))

(define tags-key 'tags)

(define (register-property-tags item-data)
  (data:new-property item-data #:key tags-key #:name "Tags" #:default (val:make-set)))

(define (get-tags item-data item)
  (data:get-property item-data item tags-key))
