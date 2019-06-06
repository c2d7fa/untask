#lang racket

(provide execute)

(require
 (prefix-in item: "../data/item-data.rkt")
 (prefix-in export: "../data/export.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")

 "../data/context.rkt"
 "../data/state.rkt"

 "../data/filter-expressions.rkt"
 "../data/modify-expressions.rkt"

 (prefix-in a: "../util/attributes.rkt")
 (only-in "../util.rkt" thread))


(define (filter-expression-with-contexts filter-expression state)
  (foldl (λ (context-name filter-expression)
           (apply-context-filter (a:get (state
                                         state.defined-contexts
                                         (contexts.named context-name)))
                                 filter-expression))
         filter-expression
         (set->list (a:get (state state.active-contexts)))))

(define (modify-expression-with-contexts modify-expression state)
  (foldl (λ (context-name filter-expression)
           (apply-context-modify (a:get (state
                                         state.defined-contexts
                                         (contexts.named context-name)))
                                 modify-expression))
         modify-expression
         (set->list (a:get (state state.active-contexts)))))

(define (execute-list state filter-expression #:property-types property-types)
  (let ((item-data (a:get (state state.item-data))))
    `((list-items ,item-data
                  ,(urgency:sort-items-by-urgency-descending
                    item-data
                    (search item-data
                            (filter-expression-with-contexts filter-expression state)
                            #:property-types property-types))))))

(define (execute-add state modify-expression #:property-types property-types)
  (let-values (((item-data-with-new-item new-item)
                (item:new-item (a:get (state state.item-data)))))
    (let ((new-state
           (a:set (state state.item-data)
                  (evaluate-modify-expression #:property-types property-types
                                              (modify-expression-with-contexts modify-expression
                                                                               state)
                                              item-data-with-new-item
                                              new-item))))
      `((set-state ,new-state)
        (list-items ,(a:get (new-state state.item-data)) (,new-item))))))

(define (execute-modify state filter-expression modify-expression #:property-types property-types)
  (let ((new-state
         (a:update (state state.item-data)
                   (λ (item-data)
                     (modify-items item-data
                                   (search item-data #:property-types property-types
                                           (filter-expression-with-contexts filter-expression state))
                                   (modify-expression-with-contexts modify-expression state)
                                   #:property-types property-types)))))
    `((set-state ,new-state)
      (list-items ,(a:get (state state.item-data))
                  ,(set->list (search (a:get (new-state state.item-data)) #:property-types property-types
                                      (filter-expression-with-contexts filter-expression state)))))))

(define (execute command-line-representation state #:property-types property-types)
  (match command-line-representation
    (`(,filter-expression list)
     (execute-list state filter-expression #:property-types property-types))
    (`(add ,modify-expression)
     (execute-add state modify-expression #:property-types property-types))
    (`(,filter-expression modify ,modify-expression)
     (execute-modify state filter-expression modify-expression #:property-types property-types))
    (`(context show)
     `((print-raw ,(format "~a" (available-contexts (a:get (state state.defined-contexts)))))))
    (`(context add ,name ,filter-expression ,modify-expression)
     `((set-state ,(a:set (state state.defined-contexts (contexts.named name))
                          (context #:filter filter-expression
                                   #:modify modify-expression)))))
    (`(context remove ,name)
     `((set-state ,(a:update (state state.defined-contexts)
                             (λ (x) (remove-context x name))))))
    (`(context active ,toggles)
     `((set-state ,(a:update (state state.active-contexts)
                             (λ (active-contexts)
                               (foldl (λ (expr active-contexts)
                                        (match expr
                                          (`(on ,name)  (set-add active-contexts name))
                                          (`(off ,name) (set-remove active-contexts name))))
                                      active-contexts
                                      toggles))))))
    (`(save ,filename)
     `((write-file ,filename ,(export:export-item-data-to-string (a:get (state state.item-data))))))
    (`(load ,filename)
     `((load-item-data-from-file ,filename)))))
