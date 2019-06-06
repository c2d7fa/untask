#lang racket

(provide build-state
         get-descriptions
         run-list-command
         )

(require
 "../src/data/state.rkt"
 "../src/data/property-type.rkt"
 "../src/user/execute.rkt"
 "../src/user/parser.rkt"
 "../src/util.rkt"

 "../src/properties/status.rkt"
 "../src/properties/description.rkt"
 "../src/properties/tags.rkt"
 "../src/properties/urgency.rkt"
 "../src/properties/dependencies.rkt"

 (prefix-in a: "../src/util/attributes.rkt")
 (prefix-in i: "../src/data/item-data.rkt")
 (prefix-in v: "../src/data/values.rkt")
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
