#lang racket

(provide interpret interpret-string)

(require
 "../core/state.rkt"
 (prefix-in item: "../core/item.rkt")
 (prefix-in export: "../core/export.rkt")
 (prefix-in urgency: "../properties/urgency.rkt")

 "../properties/builtin.rkt"

 "./filter.rkt"
 "./modify.rkt"
 "../core/context.rkt"
 "./check-expression.rkt"

 (prefix-in parser: "../user/parser.rkt")
 (prefix-in term: "../../terminal.rkt")
 (only-in "../user/render-list.rkt" render-listing render-listing-info)
 (prefix-in a: "../../attribute.rkt")
 (only-in "../../misc.rkt" thread)
 )

;; TODO: This should probably not be allowed to read file directly.
(define (has-unsaved-state? state)
  (not (or (not (a:get (state state.open-file)))
           (and (file-exists? (a:get (state state.open-file)))
                (equal? (export:read-state-from-file (a:get (state state.open-file)))
                        state)))))

(define (interpret-check-expression fm-expression filter? continue)
  (let ((check-value (check-filter/modify-expression fm-expression filter?)))
    (if (eq? #t check-value)
        (continue)
        `(error ,check-value ,(λ () '(value proceed))))))

(define (contextify-expression fm-expression filter? state)
  (let ((apply-context (if filter? apply-context-filter apply-context-modify)))
    (foldl (λ (context-name fm-expression)
             (apply-context (a:get (state
                                    state.defined-contexts
                                    (contexts.named context-name)))
                            fm-expression))
           fm-expression
           (set->list (a:get (state state.active-contexts))))))

(define (interpret-check-contextify-expression fm-expression filter? continue)
  (interpret-check-expression fm-expression filter?
                              (λ ()
                                `(get-state ,(λ (state)
                                               (continue state (contextify-expression fm-expression filter? state)))))))


(define (interpret-search filter-expression continue)
  (interpret-check-contextify-expression filter-expression #t
                                         (λ (state filter-expression)
                                           (continue state (search (a:get (state state.item-data))
                                                                   filter-expression)))))

(define (interpret-list filter-expression continue)
  (interpret-search filter-expression
                    (λ (state matching-items)
                      `(list-items ,state
                                   ,(urgency:sort-items-by-urgency-descending
                                     (a:get (state state.item-data))
                                     matching-items)
                                   ,continue))))

(define (interpret-info filter-expression continue)
  (interpret-search filter-expression
                    (λ (state matching-items)
                      `(info-items ,state
                                   ,(urgency:sort-items-by-urgency-descending
                                     (a:get (state state.item-data))
                                     matching-items)
                                   ,continue))))

(define (interpret-add modify-expression continue)
  (interpret-check-contextify-expression
   modify-expression #f
   (λ (state modify-expression)
     (let-values (((item-data-with-new-item new-item) (item:new-item (a:get (state state.item-data)))))
       (let ((new-state (a:set (state state.item-data)
                               (evaluate-modify-expression modify-expression
                                                           item-data-with-new-item
                                                           new-item))))
         `(set-state ,new-state ,(λ () `(list-items ,new-state (,new-item) ,continue))))))))

(define (interpret-modify filter-expression modify-expression continue)
  (interpret-check-contextify-expression modify-expression #f
                                         (λ (state modify-expression)
                                           (interpret-search filter-expression
                                                             (λ (state matching-items)
                                                               (let ((new-state (a:update (state state.item-data)
                                                                                          (λ (item-data)
                                                                                            (modify-items item-data
                                                                                                          matching-items
                                                                                                          modify-expression)))))
                                                                 `(set-state ,new-state ,(λ () `(list-items ,new-state ,(set->list matching-items) ,continue)))))))))

(define (interpret-remove filter-expression continue)
  (interpret-search filter-expression
                    (λ (state matching-items)
                      `(set-state ,(a:update (state state.item-data)
                                             (λ (item-data)
                                               (foldl (λ (item item-data)
                                                        (item:remove-item item-data item))
                                                      item-data
                                                      (set->list matching-items))))
                                  ,continue))))

;; Takes a command (the value returned by parse) and returns an interpretation
;; whose value is one of 'proceed and 'quit.
;;
;; An interpretation is a Racket expression which is one of the following:
;; - (value x)
;; - (get-state (λ (state) ...))
;; - (set-state state (λ () ...))
;; - (read-file filename (λ (data) ...))
;; - (write-file filename data (λ () ...))
;; - (confirm prompt (λ (confirmed?) ...))
;; - (list-items state items (λ () ...))
;; - (info-items state items (λ () ...))
;; - (message string (λ () ...))
;;
;; The interpretation (value x) represents the simple value x. In each other
;; case, the interpretation represents some operation, whose behavior is defined
;; elsewhere, associated with a function that consumes the result of that
;; operation and returns the next operation.
(define (interpret command)
  (match command
    (`(,filter-expression list)
     (interpret-list filter-expression (λ () '(value proceed))))
    (`(add ,modify-expression)
     (interpret-add modify-expression (λ () '(value proceed))))
    (`(,filter-expression modify ,modify-expression)
     (interpret-modify filter-expression modify-expression (λ () '(value proceed))))
    (`(,filter-expression remove)
     (interpret-remove filter-expression (λ () '(value proceed))))
    (`(,filter-expression info)
     (interpret-info filter-expression (λ () '(value proceed))))
    (`(open ,filename)
     (let ((load-state (λ (old-state file-state)
                         (thread old-state
                                 ((λ (state) (a:set (state state.open-file) filename)))
                                 ((λ (state) (a:set (state state.item-data)
                                                    (a:get (file-state state.item-data)))))
                                 ((λ (state) (a:set (state state.defined-contexts)
                                                    (a:get (file-state state.defined-contexts)))))
                                 ((λ (state) (a:set (state state.active-contexts)
                                                    (a:get (file-state state.active-contexts)))))))))
     `(get-state
       ,(λ (state)
          `(read-file ,filename
                      ,(λ (file-content)
                         (if (has-unsaved-state? state)
                             `(confirm "You have unsaved data. Proceed?"
                                       ,(λ (answer)
                                          (if answer
                                              `(set-state ,(load-state state (export:read-state-from-string file-content)) ,(λ () '(value proceed)))
                                              `(value proceed))))
                             `(set-state ,(load-state state (export:read-state-from-string file-content)) ,(λ () '(value proceed))))))))))
    (`(save)
     `(get-state
       ,(λ (state)
          `(write-file ,(a:get (state state.open-file)) ,(export:export-state-to-string state) ,(λ () '(value proceed))))))
    (`(exit)
     `(get-state ,(λ (state) (if (has-unsaved-state? state)
                                 `(confirm "You have unsaved data. Proceed?"
                                           ,(λ (answer)
                                              (if answer
                                                  '(value exit)
                                                  '(value proceed))))
                                 '(value exit)))))
    (`(context add ,name ,filter-expression ,modify-expression)
     `(get-state ,(λ (state) `(set-state ,(a:set (state state.defined-contexts (contexts.named name))
                                                 (context #:filter filter-expression
                                                          #:modify modify-expression))
                                         ,(λ () '(value proceed))))))
    (`(context active ,toggles)
     `(get-state ,(λ (state)
                    `(set-state ,(a:update (state state.active-contexts)
                                           (λ (active-contexts)
                                             (foldl (λ (expr active-contexts)
                                                      (match expr
                                                        (`(on ,name)  (set-add active-contexts name))
                                                        (`(off ,name) (set-remove active-contexts name))))
                                                    active-contexts
                                                    toggles)))
                                ,(λ () '(value proceed))))))
    (`(context show)
     `(get-state ,(λ (state)
                    `(message ,(format "~a" (available-contexts (a:get (state state.defined-contexts)))) ,(λ () '(value proceed))))))
    (`(context remove ,name)
     `(get-state ,(λ (state)
                    `(set-state ,(a:update (state state.defined-contexts)
                                           (λ (x) (remove-context x name)))
                                ,(λ () '(value proceed))))))))

;; Parse input string and interpret it like `interpret'. If input is #f, return
;; 'exit value. If input cannot be parsed, print a human-readable error instead.
(define (interpret-string input)
  (with-handlers ((exn:fail:read? (λ (e) `(error "Unable to parse command." ,(λ () '(value proceed))))))
    (if input (interpret (parser:parse input)) `(value exit))))
