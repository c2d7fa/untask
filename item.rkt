#lang racket

(require (prefix-in state: "state.rkt"))

(require (for-syntax racket/syntax))

(provide (all-defined-out))

(struct item (id state))

(define-syntax (define-item-property stx)
  (syntax-case stx ()
    ((_ property-name)
     (with-syntax ((defined-identifier (format-id #'property-name "item-~a" #'property-name))
                   (getter-identifier (format-id stx "state:get-~a" #'property-name)))
       #'(define (defined-identifier item)
           (getter-identifier (item-id item) (item-state item)))))))

(define-item-property status)
(define-item-property tags)
(define-item-property description)

(define (all-items state)
  (set-map (state:all-ids state)
           (λ (id)
             (item id state))))
