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

(define (enrich-with-contexts apply-context expression state)
  (foldl (λ (context-name expression)
             (apply-context (a:get (state
                                    state.defined-contexts
                                    (contexts.named context-name)))
                            expression))
           expression
           (set->list (a:get (state state.active-contexts)))))

;; Enrich filter-expression with contexts and report errors. If no errors are
;; found, call continue with filter expression. If state is #f, then contexts
;; are not used.
(define (execute-with-filter-expression filter-expression state continue #:property-types property-types)
  (let* ((filter-expression (if state (enrich-with-contexts apply-context-filter filter-expression state) filter-expression))
         (check-value (check-filter-expression filter-expression #:property-types property-types)))
    (if (eq? #t check-value)
        (continue filter-expression)
        `((error ,check-value)))))

;; Same as execute-with-filter-expression, but for modify expressions.
(define (execute-with-modify-expression modify-expression state continue #:property-types property-types)
  (let* ((modify-expression (if state (enrich-with-contexts apply-context-modify modify-expression state) modify-expression))
         (check-value (check-modify-expression modify-expression #:property-types property-types)))
    (if (eq? #t check-value)
        (continue modify-expression)
        `((error ,check-value)))))

(define (execute-with-filter-and-modify-expressions filter-expression modify-expression state continue #:property-types property-types)
  (execute-with-filter-expression filter-expression #:property-types property-types
                                  state
                                  (λ (filter-expression)
                                    (execute-with-modify-expression #:property-types property-types
                                     modify-expression
                                     state
                                     (λ (modify-expression)
                                       (continue filter-expression modify-expression))))))

(define (execute-list state filter-expression #:property-types property-types)
  (define (execute-list* filter-expression)
    (let ((item-data (a:get (state state.item-data))))
        `((list-items ,item-data
                      ,(urgency:sort-items-by-urgency-descending
                        item-data
                        (search item-data
                                filter-expression
                                #:property-types property-types))))))
  (execute-with-filter-expression filter-expression state execute-list* #:property-types property-types))

(define (execute-add state modify-expression #:property-types property-types)
  (define (execute-add* modify-expression)
    (let-values (((item-data-with-new-item new-item)
                    (item:new-item (a:get (state state.item-data)))))
        (let ((new-state
               (a:set (state state.item-data)
                      (evaluate-modify-expression #:property-types property-types
                                                  modify-expression
                                                  item-data-with-new-item
                                                  new-item))))
          `((set-state ,new-state)
            (list-items ,(a:get (new-state state.item-data)) (,new-item))))))
  (execute-with-modify-expression modify-expression state execute-add* #:property-types property-types))

(define (execute-modify state filter-expression modify-expression #:property-types property-types)
  (define (execute-modify* filter-expression modify-expression)
    (let ((new-state
           (a:update (state state.item-data)
                     (λ (item-data)
                       (modify-items item-data
                                     (search item-data
                                             filter-expression
                                             #:property-types property-types)
                                     modify-expression
                                     #:property-types property-types)))))
      `((set-state ,new-state)
        (list-items ,(a:get (new-state state.item-data))
                    ,(set->list (search (a:get (state state.item-data)) #:property-types property-types
                                        filter-expression))))))
  (execute-with-filter-and-modify-expressions filter-expression modify-expression state execute-modify* #:property-types property-types))

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
     (execute-with-filter-and-modify-expressions filter-expression modify-expression #f #:property-types property-types
       (λ (filter-expression modify-expression)
         `((set-state ,(a:set (state state.defined-contexts (contexts.named name))
                          (context #:filter filter-expression
                                   #:modify modify-expression)))))))
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
