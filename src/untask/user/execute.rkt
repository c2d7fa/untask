#lang racket

(provide execute!)

(require
 "../core/state.rkt"
 "../core/context.rkt"
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in export: "../core/export.rkt")

 "../command/check-expression.rkt"
 (prefix-in filter: "../command/filter.rkt")
 (prefix-in modify: "../command/modify.rkt")

 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")
 (prefix-in date: "../properties/date.rkt")

 "../user/render-list.rkt"

 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")
 (prefix-in dt: "../../datetime.rkt")
 (only-in "../../misc.rkt" try-read-line thread))

;; For convenience, the internal procedures in this module pass boxed state
;; around as a parameter. Exceptions are used for error reporting.

(define *state (make-parameter #f))
(define *context-toggles (make-parameter #f))  ; Contexts enabled just for this command

(define (state)
  (unbox (*state)))

(define (item-data)
  (a:get ((unbox (*state)) state.item-data)))

(define (toggled-contexts toggles)
  (let ((global (a:get ((state) state.active-contexts))))
    (list->set (foldl (λ (expr active-contexts)
                        (match expr
                          (`(on ,name) (set-add active-contexts name))
                          (`(off ,name) (set-remove active-contexts name))
                          (`(reset) (set))))
                      global
                      toggles))))

(define (current-contexts)
  (let ((toggles (*context-toggles))
        (global (a:get ((state) state.active-contexts))))
    (if toggles
        (toggled-contexts toggles)
        global)))

;; Check whether fme is a valid filter or modify expression. If it isn't, throw
;; an exception.
(define (check! fme #:filter? filter?)
  (let ((check (check-filter/modify-expression fme filter?)))
    (when (not (eq? #t check))
      (raise-user-error check))))

;; Extend a filter or modify expression with the currently enabled contexts.
(define (with-contexts fme #:filter? filter?)
  (let ((apply-context (if filter? apply-context-filter apply-context-modify)))
    (foldl (λ (context-name fme)
             (apply-context (a:get ((state)
                                    state.defined-contexts
                                    (contexts.named context-name)))
                            fme))
           fme
           (set->list (current-contexts)))))

;; Find all items matching filter expression exactly.
(define (search* fe)
  (filter:search (item-data) fe))

;; Like search*, but automatically uses current contexts and returns list sorted
;; by urgency rather than set.
(define (search fe)
  (urgency:sort-items-by-urgency-descending
   (item-data)
   (search* (with-contexts fe #:filter? #t))))

(define (list! items)
  (displayln (render-listing (item-data) items)))

(define (read-file! path)
  (and path (file-exists? path)
       (port->string (open-input-file path) #:close? #t)))

(define (write-file! path content)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (display content out))))

(define (display-message! message)
  (displayln (term:render `((blue) (,message)))))

(define (confirm-unsaved!)
  (let ((saved (read-file! (a:get ((state) state.open-file)))))
    (if (or (not saved)
            (equal? (export:read-state-from-string saved) (state)))
        #t
        (begin
          (display (term:render `((bold) (blue) ("You have unsaved data. Proceed? "))))
          (let ((input (try-read-line)))
            (when (not input) (displayln ""))
            (or (not input)
                (string-prefix? input "y")
                (string-prefix? input "Y")))))))

(define (execute-list! fe)
  (check! fe #:filter? #t)
  (list! (search fe)))

(define (execute-info! fe)
  (check! fe #:filter? #t)
  (displayln (render-listing-info (item-data) (search fe))))

(define (execute-tree! fe rhs)
  (check! fe #:filter? #t)
  (check! rhs #:filter? #t)
  (define (build-tree item-data item)
    (cons item (map (λ (i) (build-tree item-data (val:unwrap-item i)))
                    (filter (λ (i)
                              (filter:evaluate-filter-expression rhs item-data (val:unwrap-item i)))
                            (set->list (val:unwrap-set (item:get-property item-data item depends:depends-property-type)))))))
  (displayln (render-trees (item-data)
                           (map (λ (item)
                                  (build-tree (item-data) item))
                                (search fe)))))

(define (execute-agenda! fe)
  (check! fe #:filter? #t)
  (displayln (render-agenda (item-data)
                            (sort (foldl (λ (item blocks)
                                           (let ((item-date (val:unwrap-date (item:get-property (item-data)
                                                                                                item
                                                                                                date:date-property-type))))
                                             (if (assoc item-date blocks dt:same-date?)
                                                 (map (λ (block)
                                                        (if (dt:same-date? (car block) item-date)
                                                            `(,@block ,item)
                                                            block))
                                                      blocks)
                                                 `(,@blocks (,item-date ,item)))))
                                         (list)
                                         (search `(and (not (date : #f)) ,fe)))
                                 #:key car dt:date-before?))))

(define (execute-add! me)
  (check! me #:filter? #f)
  (let-values (((item-data-with-new-item new-item) (item:new-item (a:get ((state) state.item-data)))))
    (set-box! (*state)
              (a:set ((state) state.item-data)
                     (modify:evaluate-modify-expression (with-contexts me #:filter? #f)
                                                        item-data-with-new-item new-item)))
    (list! (list new-item))))

(define (execute-modify! fe me)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (let ((items (search fe)))
    (set-box! (*state)
              (a:update ((state) state.item-data)
                        (λ (item-data)
                          (modify:modify-items item-data (list->set items) me))))
    (list! items)))

(define (execute-remove! fe)
  (check! fe #:filter? #t)
  (set-box! (*state)
            (a:update ((state) state.item-data)
                      (λ (item-data)
                        (foldl (λ (item item-data)
                                 (item:remove-item item-data item))
                               item-data
                               (search fe))))))

;; Execute a command and update the state by setting the boxed state. Return
;; 'exit if the given command should cause the program to exit, and return
;; 'proceed otherwise.
(define (execute! command state-box)
  (parameterize ((*state state-box))
    (with-handlers ((exn:fail:user?
                     (λ (e) (displayln (term:render `((bold) (red) ("Error: " ,(exn-message e))))) 'proceed)))
      (match command
        (`(,fe list)       (execute-list! fe)      'proceed)
        (`(add ,me)        (execute-add! me)       'proceed)
        (`(,fe modify ,me) (execute-modify! fe me) 'proceed)
        (`(,fe remove)     (execute-remove! fe)    'proceed)
        (`(,fe agenda)     (execute-agenda! fe)    'proceed)
        (`(,fe tree ,rhs)  (execute-tree! fe rhs)  'proceed)
        (`(open ,filename)
         ;; TODO: Our whole system for saving and loading data is a mess.
         (let ((load-state (λ (old-state file-state)
                             (thread old-state
                                     ((λ (state) (a:set (state state.open-file) filename)))
                                     ((λ (state) (a:set (state state.item-data)
                                                        (a:get (file-state state.item-data)))))
                                     ((λ (state) (a:set (state state.defined-contexts)
                                                        (a:get (file-state state.defined-contexts)))))
                                     ((λ (state) (a:set (state state.active-contexts)
                                                        (a:get (file-state state.active-contexts)))))))))
           (let ((file-content (read-file! filename))
                 (confirm? (confirm-unsaved!)))
             (when confirm?
                 (if file-content
                     (set-box! (*state) (load-state (state) (export:read-state-from-string file-content)))
                     (set-box! (*state) (a:set (state-empty state.open-file) filename))))))
         'proceed)
        (`(save)
         (write-file! (a:get ((state) state.open-file)) (export:export-state-to-string (state)))
         'proceed)
        (`(context active ,toggles)
         (set-box! (*state) (a:set ((state) state.active-contexts)
                                   (toggled-contexts toggles)))
         'proceed)
        (`(context add ,name ,fe ,me)
         (check! fe #:filter? #t)
         (check! me #:filter? #f)
         (set-box! (*state) (a:set ((state) state.defined-contexts (contexts.named name))
                                   (context #:filter fe
                                            #:modify me)))
         'proceed)
        (`(context remove ,name)
         (set-box! (*state) (a:update ((state) state.defined-contexts)
                                      (λ (x) (remove-context x name))))
         'proceed)
        (`(context show)
         (display-message! (format "~a" (available-contexts (a:get ((state) state.defined-contexts)))))
         'proceed)
        (`(with-contexts ,toggles ,subcommand)
         (parameterize ((*context-toggles toggles))
           (execute! subcommand state-box)))
        (`(exit)
         (if (confirm-unsaved!) 'exit 'proceed))))))