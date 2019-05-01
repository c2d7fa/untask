#lang racket

(provide (all-defined-out))

(require (prefix-in data: "item-data.rkt"))

(define tags-key 'tags)

(define (register-property-tags item-data)
  (data:new-property item-data #:key tags-key #:name "Tags" #:default (set)))

(define (has-tag? item-data item tag)
  (set-member? (data:get-property item-data item tags-key) tag))

(define (add-tag item-data item tag)
  (data:update-property item-data item tags-key (λ (tags) (set-add tags tag))))

(define (remove-tag item-data item tag)
  (data:update-property item-data item tags-key (λ (tags) (set-remove tags tag))))
