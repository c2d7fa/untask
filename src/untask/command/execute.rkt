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

 (prefix-in a: "../../attribute.rkt")
 (only-in "../../misc.rkt" thread))


(define (filter-expression-with-contexts filter-expression state)
  (foldl (λ (context-name filter-expression)
           (apply-context-filter (a:get (state
                                         state.defined-contexts
                                         (contexts.named context-name)))
                                 filter-expression))
         filter-expression
         (set->list (a:get (state state.active-contexts)))))

(define (modify-expression-with-contexts modify-expression state)
  (foldl (λ (context-name modify-expression)
           (apply-context-modify (a:get (state
                                         state.defined-contexts
                                         (contexts.named context-name)))
                                 modify-expression))
         modify-expression
         (set->list (a:get (state state.active-contexts)))))

(define (execute-list state filter-expression #:property-types property-types)
  (if (eq? #t (check-filter-expression filter-expression #:property-types property-types))
      (let ((item-data (a:get (state state.item-data))))
        `((list-items ,item-data
                      ,(urgency:sort-items-by-urgency-descending
                        item-data
                        (search item-data
                                (filter-expression-with-contexts filter-expression state)
                                #:property-types property-types)))))
      `((error ,(check-filter-expression filter-expression #:property-types property-types)))))

(define (execute-add state modify-expression #:property-types property-types)
  (if (eq? #t (check-modify-expression modify-expression #:property-types property-types))
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
            (list-items ,(a:get (new-state state.item-data)) (,new-item)))))
      `((error ,(check-modify-expression modify-expression #:property-types property-types)))))

(define (execute-modify state filter-expression modify-expression #:property-types property-types)
  (if (eq? #t (check-filter-expression filter-expression #:property-types property-types))
      (if (eq? #t (check-modify-expression modify-expression #:property-types property-types))
          (let ((new-state
                 (a:update (state state.item-data)
                           (λ (item-data)
                             (modify-items item-data
                                           (search item-data #:property-types property-types
                                                   (filter-expression-with-contexts filter-expression state))
                                           (modify-expression-with-contexts modify-expression state)
                                           #:property-types property-types)))))
            `((set-state ,new-state)
              (list-items ,(a:get (new-state state.item-data))
                          ,(set->list (search (a:get (state state.item-data)) #:property-types property-types
                                              (filter-expression-with-contexts filter-expression state))))))
          `((error ,(check-modify-expression modify-expression #:property-types property-types))))
      `((error ,(check-filter-expression filter-expression #:property-types property-types)))))

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
