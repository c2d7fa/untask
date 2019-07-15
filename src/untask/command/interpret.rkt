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
 (prefix-in a: "../../attribute.rkt")
 (only-in "../../misc.rkt" thread)
 )

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

;; Basic interpretations
(define (get-state/i continue) `(get-state ,continue))
(define (set-state/i state continue) `(set-state ,state ,continue))
(define (read-file/i filename continue) `(read-file ,filename ,continue))
(define (write-file/i filename data continue) `(write-file ,filename ,data ,continue))
(define (confirm/i prompt continue) `(confirm ,prompt ,continue))
(define (message/i message continue) `(message ,message ,continue))
(define (error/i message continue) `(error ,message ,continue))
(define (list/i state items continue) `(list-items ,state ,items ,continue))
(define (info/i state items continue) `(info-items ,state ,items ,continue))
(define (value/i value) `(value ,value))

;; This macro constructs an interpretation in a style inspired by Haskell's
;; "do"-notation.
(define-syntax (interp stx)
  (syntax-case stx ()
    ((interp (let (vars ...) (expr ...))
             stmts ...)
     #'(expr ... (λ (vars ...)
                   (interp stmts ...))))
    ((interp (do expr))
     #'expr)))

(define (check-fm/i fm-expression filter? continue)
  (let ((check-value (check-filter/modify-expression fm-expression filter?)))
    (if (eq? #t check-value)
        (continue)
        (interp (let () (error/i check-value))
                (do (value/i 'proceed))))))

(define (check-contextify-fm/i fm-expression filter? continue)
  (define (contextify-expression fm-expression filter? state)
    (let ((apply-context (if filter? apply-context-filter apply-context-modify)))
      (foldl (λ (context-name fm-expression)
               (apply-context (a:get (state
                                      state.defined-contexts
                                      (contexts.named context-name)))
                              fm-expression))
             fm-expression
             (set->list (a:get (state state.active-contexts))))))
  (interp
   (let () (check-fm/i fm-expression filter?))
   (let (state) (get-state/i))
   (do (continue (contextify-expression fm-expression filter? state)))))

(define (check-contextify-filter/i filter-expression continue)
  (check-contextify-fm/i filter-expression #t continue))

(define (check-contextify-modify/i modify-expression continue)
  (check-contextify-fm/i modify-expression #f continue))

(define (interpret-search filter-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (filter-expression) (check-contextify-filter/i filter-expression))
   (do (continue (search (a:get (state state.item-data)) filter-expression)))))

(define (search/i filter-expression continue)
  (interpret-search filter-expression continue))

(define (search-sorted/i filter-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (items) (search/i filter-expression))
   (do (continue (urgency:sort-items-by-urgency-descending
                  (a:get (state state.item-data))
                  items)))))

(define (interpret-list filter-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (items) (search-sorted/i filter-expression))
   (let () (list/i state items))
   (do (continue))))

(define (interpret-info filter-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (items) (search-sorted/i filter-expression))
   (let () (info/i state items))
   (do (continue))))

(define (interpret-add modify-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (modify-expression) (check-contextify-modify/i modify-expression))
   (do (let-values (((item-data-with-new-item new-item) (item:new-item (a:get (state state.item-data)))))
         (let ((new-state (a:set (state state.item-data)
                                 (evaluate-modify-expression modify-expression item-data-with-new-item new-item))))
           (interp
            (let () (set-state/i new-state))
            (let () (list/i new-state (list new-item)))
            (do (continue))))))))

(define (interpret-modify filter-expression modify-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (items) (search/i filter-expression))
   (let (modify-expression) (check-contextify-modify/i modify-expression))
   (do (let ((new-state (a:update (state state.item-data)
                                  (λ (item-data)
                                    (modify-items item-data items modify-expression)))))
         (interp
          (let () (set-state/i new-state))
          (let () (list/i new-state (set->list items)))
          (do (continue)))))))

(define (interpret-remove filter-expression continue)
  (interp
   (let (state) (get-state/i))
   (let (items) (search/i filter-expression))
   (let () (set-state/i (a:update (state state.item-data)
                                  (λ (item-data)
                                    (foldl (λ (item item-data)
                                             (item:remove-item item-data item))
                                           item-data
                                           (set->list items))))))
   (do (continue))))

(define (confirm-unsaved/i continue)
  (interp
   (let (state) (get-state/i))
   (let (open-file-content) (read-file/i (a:get (state state.open-file))))
   (do (if (or (not open-file-content)
               (equal? (export:read-state-from-string open-file-content) state))
           (continue #t)
           (interp
            (let (answer) (confirm/i "You have unsaved data. Proceed?"))
            (do (if answer
                    (continue #t)
                    (continue #f))))))))

;; Takes a command (the value returned by parse) and returns an interpretation
;; whose value is one of 'proceed and 'quit.
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
       (interp
        (let (state) (get-state/i))
        (let (file-content) (read-file/i filename))
        (let (confirm?) (confirm-unsaved/i))
        (do (if confirm?
                (interp (let () (set-state/i (load-state state (export:read-state-from-string file-content))))
                        (do (value/i 'proceed)))
                (value/i 'proceed))))))
    (`(save)
     (interp
      (let (state) (get-state/i))
      (let () (write-file/i (a:get (state state.open-file))
                            (export:export-state-to-string state)))
      (do (value/i 'proceed))))
    (`(exit)
     (interp
      (let (confirm?) (confirm-unsaved/i))
      (do (if confirm? (value/i 'exit) (value/i 'proceed)))))
    (`(context add ,name ,filter-expression ,modify-expression)
     (interp
      (let (state) (get-state/i))
      (let () (set-state/i (a:set (state state.defined-contexts (contexts.named name))
                                  (context #:filter filter-expression
                                           #:modify modify-expression))))
      (do (value/i 'proceed))))
    (`(context active ,toggles)
     (interp
      (let (state) (get-state/i))
      (let () (set-state/i (a:update (state state.active-contexts)
                                     (λ (active-contexts)
                                       (foldl (λ (expr active-contexts)
                                                (match expr
                                                  (`(on ,name)  (set-add active-contexts name))
                                                  (`(off ,name) (set-remove active-contexts name))))
                                              active-contexts
                                              toggles)))))
      (do (value/i 'proceed))))
    (`(context show)
     (interp
      (let (state) (get-state/i))
      (let () (message/i (format "~a" (available-contexts (a:get (state state.defined-contexts))))))
      (do (value/i 'proceed))))
    (`(context remove ,name)
     (interp
      (let (state) (get-state/i))
      (let () (set-state/i (a:update (state state.defined-contexts)
                                     (λ (x) (remove-context x name)))))
      (do (value/i 'proceed))))))

;; Parse input string and interpret it like `interpret'. If input is #f, return
;; 'exit value. If input cannot be parsed, print a human-readable error instead.
(define (interpret-string input)
  (with-handlers ((exn:fail:read? (λ (e) `(error "Unable to parse command." ,(λ () '(value proceed))))))
    (if input (interpret (parser:parse input)) `(value exit))))
