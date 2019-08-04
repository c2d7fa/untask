#lang racket

(provide wait-property-type
         wait-active?)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in prop: "../core/property.rkt")
 (prefix-in val:  "../core/value.rkt")

  (prefix-in g: gregor))

(define wait-property-type
  (prop:make-property-type #:key 'wait
                           #:type '(opt date)
                           #:default #f))

(define (wait-active? item-data item)
  (let ((prop (item:get-property item-data item wait-property-type)))
    (match prop
      (#f #t)
      (`(date ,year ,month ,day #f #f) (g:date<=? (g:date year month day) (g:today)))
      (`(date ,year ,month ,day ,hour ,minute) (g:datetime<=? (g:datetime year month day hour minute) (g:now)))
      (else (error "Unexpected case")))))
