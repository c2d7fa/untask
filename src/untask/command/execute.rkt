#lang racket

(provide execute)

(require
 (prefix-in item: "../core/item.rkt")
 (prefix-in export: "../core/export.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")

 "../core/context.rkt"
 "../core/state.rkt"

 "filter.rkt"
 "modify.rkt"
 "check-expression.rkt"

 (prefix-in a: "../../attribute.rkt")
 (only-in "../../misc.rkt" thread))

;; Helper functions

(define (enrich-with-contexts expression filter? state)
  (let ((apply-context (if filter? apply-context-filter apply-context-modify)))
    (foldl (λ (context-name expression)
             (apply-context (a:get (state
                                    state.defined-contexts
                                    (contexts.named context-name)))
                            expression))
           expression
           (set->list (a:get (state state.active-contexts))))))

(define (execute-with fm-expression filter? continue #:property-types property-types)
  (let ((check-value (check-filter/modify-expression fm-expression filter? #:property-types property-types)))
    (if (eq? #t check-value)
        (continue fm-expression)
        `((error ,check-value)))))

(define (execute-with-enriched fm-expression filter? state continue #:property-types property-types)
  (execute-with (enrich-with-contexts fm-expression filter? state) filter? #:property-types property-types
    continue))

(define (execute-with-search filter-expression state continue #:property-types property-types)
  (execute-with-enriched filter-expression #t state #:property-types property-types
    (λ (filter-expression)
      (continue (search (a:get (state state.item-data)) filter-expression #:property-types property-types)))))

;; --

(define (execute-list state filter-expression #:property-types property-types)
  (execute-with-search filter-expression state #:property-types property-types
    (λ (items)
      `((list-items ,(a:get (state state.item-data))
                    ,(urgency:sort-items-by-urgency-descending
                      (a:get (state state.item-data))
                      items))))))

(define (execute-add state modify-expression #:property-types property-types)
  (execute-with-enriched modify-expression #f state #:property-types property-types
    (λ (modify-expression)
      (let-values (((item-data-with-new-item new-item)
                    (item:new-item (a:get (state state.item-data)))))
        (let ((new-state
               (a:set (state state.item-data)
                      (evaluate-modify-expression #:property-types property-types
                                                  modify-expression
                                                  item-data-with-new-item
                                                  new-item))))
          `((set-state ,new-state)
            (list-items ,(a:get (new-state state.item-data)) (,new-item))))))))

(define (execute-modify state filter-expression modify-expression #:property-types property-types)
  (execute-with-search filter-expression state #:property-types property-types
    (λ (items)
      (execute-with-enriched modify-expression #f state #:property-types property-types
        (λ (modify-expression)
        (let ((new-state (a:update (state state.item-data)
                                   (λ (item-data)
                                     (modify-items item-data #:property-types property-types
                                                   items
                                                   modify-expression)))))
          `((set-state ,new-state)
            (list-items ,(a:get (new-state state.item-data))
                        ,(set->list items)))))))))

(define (execute-remove state filter-expression #:property-types property-types)
  (execute-with-search filter-expression state #:property-types property-types
    (λ (items)
      `((set-state ,(a:update (state state.item-data)
                              (λ (item-data)
                                (foldl (λ (item item-data)
                                         (item:remove-item item-data item))
                                       item-data
                                       (set->list items)))))))))

(define (execute command-line-representation state #:property-types property-types)
  (match command-line-representation
    (`(,filter-expression list)
     (execute-list state filter-expression #:property-types property-types))
    (`(add ,modify-expression)
     (execute-add state modify-expression #:property-types property-types))
    (`(,filter-expression modify ,modify-expression)
     (execute-modify state filter-expression modify-expression #:property-types property-types))
    (`(,filter-expression remove)
     (execute-remove state filter-expression #:property-types property-types))
    (`(context show)
     `((print-raw ,(format "~a" (available-contexts (a:get (state state.defined-contexts)))))))
    (`(context add ,name ,filter-expression ,modify-expression)
     (execute-with (enrich-with-contexts filter-expression #t state) #t #:property-types property-types
       (λ (filter-expression)
         (execute-with (enrich-with-contexts modify-expression #f state) #f #:property-types property-types
           (λ (modify-expression)
             `((set-state ,(a:set (state state.defined-contexts (contexts.named name))
                                  (context #:filter filter-expression
                                           #:modify modify-expression)))))))))
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
    (`(save)
     `((write-file ,(a:get (state state.open-file)) ,(export:export-state-to-string state))))
    (`(open ,filename)
     `((load-state-from-file-and-then ,filename
                                      ,(λ (loaded-state)
                                         `(set-state ,(thread state
                                                              ((λ (state) (a:set (state state.open-file) filename)))
                                                              ((λ (state) (a:set (state state.item-data)
                                                                                 (a:get (loaded-state state.item-data)))))
                                                              ((λ (state) (a:set (state state.defined-contexts)
                                                                                 (a:get (loaded-state state.defined-contexts)))))
                                                              ((λ (state) (a:set (state state.active-contexts)
                                                                                 (a:get (loaded-state state.active-contexts)))))))))))))