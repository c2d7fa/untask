#lang racket

(require rackunit
         #;""
         )

(define/provide-test-suite integration-tests
  (let-values (((state output) (run "add description:{New item}"
                                    "add description:{Other item}"
                                    "list")))
    (check-equal? output '(listing 0 1))
    (check-equal? (a:get (state state.item-data (item-data.property-of 0 description-property-type)))
                  "New item")
    (check-equal? (a:get (state state.item-data (item-data.property-of 1 description-property-type)))
                  "Other item")))
