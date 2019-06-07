#lang racket

(provide build-state
         get-descriptions
         run-list-command
         )

(require
 "../src/untask/core/state.rkt"
 "../src/untask/core/property.rkt"
 "../src/untask/command/execute.rkt"
 "../src/untask/user/parser.rkt"
 "../src/misc.rkt"

 "../src/untask/properties/status.rkt"
 "../src/untask/properties/description.rkt"
 "../src/untask/properties/tags.rkt"
 "../src/untask/properties/urgency.rkt"
 "../src/untask/properties/dependencies.rkt"

 (prefix-in a: "../src/attribute.rkt")
 (prefix-in i: "../src/untask/core/item.rkt")
 (prefix-in v: "../src/untask/core/value.rkt")
 )

(define standard-property-types
  (thread-first empty-property-type-collection
    (add-property-type status-property-type)
    (add-property-type description-property-type)
    (add-property-type tags-property-type)
    (add-property-type urgency-property-type)
    (add-property-type depends-property-type)
    (add-property-type blocks-property-type)
    ))

(define (get-action-of-type type execute-result)
  (car (filter (λ (action) (eq? (car action) type))
               execute-result)))

(define (build-state . commands)
  (foldl (λ (command state)
           (cadr (get-action-of-type 'set-state (execute (parse command) state #:property-types standard-property-types))))
         state-empty
         commands))

(define (run-list-command filter-string state)
  (caddr (get-action-of-type 'list-items (execute (parse (format "~a list" filter-string)) state #:property-types standard-property-types))))

(define (get-descriptions items state)
  (list->set
   (set-map items
            (λ (item)
              (v:unwrap-string
               (i:get-property (a:get (state state.item-data))
                               item
                               description-property-type))))))
