#lang racket

(provide (all-defined-out))

(require
 (prefix-in item: "../data/item-data.rkt")
 (prefix-in export: "../data/export.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in state: "../data/state.rkt")
 (prefix-in context: "../data/context.rkt")

 "../data/filter-expressions.rkt"
 "../data/modify-expressions.rkt")

;; Convert a representation of a user-inputted command-line into a function that
;; takes the current state and returns new-state and an output description
;; (currently just a list of items to print on the screen).
;;
;; command-line-representation is a representation of the parsed user input in
;; the form (filter-expression command-name arguments...)
(define ((execute command-line-representation #:property-types property-types) state)
  (match command-line-representation
    (`(,filter-expression list)
     (values state
             (urgency:sort-items-by-urgency-descending
              (state:state-item-data state)
              (search (state:state-item-data state)
                      filter-expression  ; TODO: Use context
                      #:property-types property-types))))
    (`(add ,modify-expression)
     (let-values (((item-data-with-new-item new-item)
                   (item:new-item (state:state-item-data state))))
       (values (state:state-set-item-data
                state
                ((evaluate-modify-expression modify-expression  ; TODO: Use context
                                             #:property-types property-types)
                 item-data-with-new-item
                 new-item))
               (list new-item))))
    (`(,filter-expression modify ,modify-expression)
     (values (state:state-set-item-data
              state
              (modify-items (state:state-item-data state)
                            (search (state:state-item-data state)
                                    filter-expression  ; TODO: Use context
                                    #:property-types property-types)
                            modify-expression  ; TODO: Use context
                            #:property-types property-types))
             (set->list (search (state:state-item-data state)  ; FIXME: Use new state
                                filter-expression  ; TODO: Use context
                                #:property-types property-types))))
    (`(context show)
     ;; TODO: Improve output; mark active context.
     ;; TODO: Return context list as output instead of printing directly.
     (writeln (context:context-definitions-available (state:state-defined-contexts state)))
     (values state (list)))
    (`(context add ,name ,filter-expression ,modify-expression)
     ;; TODO: Maybe return all items in new context.
     (values (state:state-set-defined-contexts
              state
              (context:context-definitions-define
               (state:state-defined-contexts state)
               name
               filter-expression
               modify-expression))
             (list)))
    (`(context remove ,name)
     (values (state:state-set-defined-contexts
              state
              (context:context-definitions-remove
               (state:state-defined-contexts state)
               name))
             (list)))
    ;; TODO: Support toggling multiple contexts at the same time.
    (`(context active ,toggles)
     ;; TODO: Maybe return all visible items.
     (values (state:state-set-current-contexts
              state
              (foldl
               (λ (expr active)
                 (match expr
                   (`(on ,name) (set-add active name))
                   (`(off ,name) (set-remove active name))))
               (state:state-current-contexts state)
               toggles))
             (list)))
    (`(save ,filename)
     (begin
       (call-with-output-file filename #:exists 'replace
         (λ (out)
           (display (export:export-item-data-to-string (state:state-item-data state)) out)))
       (values state (set->list (item:all-items (state:state-item-data state))))))
    (`(load ,filename)
     (let ((new-item-data (export:read-item-data-from-file filename)))
       (values (state:state-set-item-data state new-item-data)
               (set->list (item:all-items new-item-data)))))))
