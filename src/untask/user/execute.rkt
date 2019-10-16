#lang racket

(provide execute!)

(require
 "../core/state.rkt"
 (prefix-in ctx: "../core/context.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")
 (prefix-in serialize: "../core/serialize.rkt")

 "../command/check-expression.rkt"
 (prefix-in filter: "../command/filter.rkt")
 (prefix-in modify: "../command/modify.rkt")

 (prefix-in urgency: "../properties/urgency.rkt")
 (prefix-in depends: "../properties/dependencies.rkt")
 (prefix-in links: "../properties/links.rkt")
 (prefix-in date: "../properties/date.rkt")

 "../user/render-list.rkt"

 (prefix-in a: "../../attribute.rkt")
 (prefix-in term: "../../terminal.rkt")
 (prefix-in dt: "../../datetime.rkt")
 (only-in "../../misc.rkt" try-read-line)
 "../../squiggle.rkt")

;; For convenience, the internal procedures in this module pass boxed state
;; around as a parameter. Error reporting is doen using exceptions.

(define *state (make-parameter #f))

(define (state)
  (unbox (*state)))

(define (item-data)
  (a:get-path ((state) state.item-data)))

;; Check whether fme is a valid filter or modify expression. If it isn't, throw
;; an exception.
(define (check! fme #:filter? filter?)
  (let ((check (check-filter/modify-expression fme filter?)))
    (when (not (eq? #t check))
      (raise-user-error check))))

;; Extend a filter or modify expression with the currently enabled contexts.
(define (with-contexts fme #:filter? filter?)
  `(and ,fme
        ,((if filter? ctx:activated-filter ctx:activated-modify)
          (a:get-path ((state) state.context-state)))))

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
  (let ((saved (read-file! (a:get-path ((state) state.open-file)))))
    (if (or (not saved)
            (equal? (serialize:load-state (a:get ((state) state.open-file))) (state)))
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

;; TODO: Create other version of "tree" that shows the entire tree, including
;; parents/blocked items. It should have some way of highlighting the current
;; item.

(define (execute-tree! fe rhs)
  (check! fe #:filter? #t)
  (check! rhs #:filter? #t)
  ;; TODO: Distinguish between children and dependencies in output
  (define seen (list))
  (define (build-tree item-data item)
    ;; TODO: This code is ugly.
    (define (build-tree* item)
      (cons item (map (λ (i)
                        (if (member i seen)
                            (let ((has-children? (not (and (null? (set->list (val:unwrap-set (item:get-property item-data (val:unwrap-item i) depends:depends-property-type))))
                                                           (null? (set->list (val:unwrap-set (item:get-property item-data (val:unwrap-item i) links:children-property-type))))))))
                              `(,(val:unwrap-item i) ,@(if has-children? '(()) '())))
                            (begin (set! seen (cons i seen))
                                   (build-tree* (val:unwrap-item i)))))
                      (filter (λ (i)
                                (filter:evaluate-filter-expression rhs item-data (val:unwrap-item i)))
                              (append (set->list (val:unwrap-set (item:get-property item-data item depends:depends-property-type)))
                                      (set->list (val:unwrap-set (item:get-property item-data item links:children-property-type))))))))
    (build-tree* item))
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
  (let-values (((item-data-with-new-item new-item) (item:new-item (a:get-path ((state) state.item-data)))))
    (set-box! (*state)
              (a:set-path ((state) state.item-data)
                          (modify:evaluate-modify-expression (with-contexts me #:filter? #f)
                                                        item-data-with-new-item new-item)))
    (list! (list new-item))))

(define (execute-modify! fe me)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (let ((items (search fe)))
    (set-box! (*state)
              (a:update-path ((state) state.item-data)
                             (λ (item-data)
                               (modify:modify-items item-data (list->set items) me))))
    (list! items)))

(define (execute-copy! fe me)
  (define (copy-item! item)
    (let-values (((item-data1 new-item) (item:new-item (a:get ((state) state.item-data)))))
      (let ((item-data2 (item:copy-item item-data1 item new-item)))
        (set-box! (*state) (a:set-path ((state) state.item-data)
                                       (modify:evaluate-modify-expression (with-contexts me #:filter? #f)
                                                                          item-data2 new-item)))
        new-item)))
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (list! (map (λ (item)
                (copy-item! item))
              (search fe))))

(define (execute-remove! fe)
  (check! fe #:filter? #t)
  (set-box! (*state)
            (a:update-path ((state) state.item-data)
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
        (`(,fe copy ,me)   (execute-copy! fe me)   'proceed)
        (`(,fe remove)     (execute-remove! fe)    'proceed)
        (`(,fe agenda)     (execute-agenda! fe)    'proceed)
        (`(,fe tree ,rhs)  (execute-tree! fe rhs)  'proceed)
        (`(open ,filename)
         (let ((file-content (read-file! filename))
               (confirm? (confirm-unsaved!)))
           (when confirm?
             (if file-content
                 (set-box! (*state) (serialize:load-state-from-string file-content #:open-file filename))
                 (set-box! (*state) (a:set-path (state-empty state.open-file) filename)))))
         'proceed)
        (`(save)
         (serialize:save-state (state))
         'proceed)
        (`(context active ,toggles)
         (set-box! (*state) (a:update-path ((state) state.context-state)
                                           (λ> (ctx:toggle toggles))))
         'proceed)
        (`(context add ,name ,fe ,me)
         (check! fe #:filter? #t)
         (check! me #:filter? #f)
         (set-box! (*state) (a:update-path ((state) state.context-state)
                                           (λ> (ctx:register name #:filter fe #:modify me))))
         'proceed)
        (`(context remove ,name)
         (set-box! (*state) (a:update-path ((state) state.context-state)
                                           (λ> (ctx:remove name))))
         'proceed)
        (`(context show)
         (display-message! (format "~a" (ctx:available-names (a:get-path ((state) state.context-state)))))
         'proceed)
        (`(with-contexts ,toggles ,subcommand)
         (let ((old-context-state (a:get-path ((state) state.context-state))))
           (set-box! (*state) (a:update-path ((state) state.context-state)
                                             (λ> (ctx:toggle toggles))))
           ;; TODO: Handle exceptions here to ensure that context state is reset
           ;; no matter what! (Comment: When testing, this doesn't seem to be a
           ;; problem, though I don't understand why; surely if there is an
           ;; error in the execute! below, then we won't be able to reset the
           ;; contexts, right?)
           (execute! subcommand (*state))
           (set-box! (*state) (a:set-path ((state) state.context-state) old-context-state))
           'proceed))
        (`(exit)
         (if (confirm-unsaved!) 'exit 'proceed))))))
