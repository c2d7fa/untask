#lang racket

(provide execute!)

(require
 untask/src/untask/core/state
 (prefix-in c: untask/src/untask/core/context)
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 (prefix-in serialize: untask/src/untask/core/serialize)

 untask/src/untask/command/check-expression
 (prefix-in cmd: untask/src/untask/command/command)
 (prefix-in filter: untask/src/untask/command/filter)
 (prefix-in modify: untask/src/untask/command/modify)

 (prefix-in urgency: untask/src/untask/properties/urgency)
 (prefix-in depends: untask/src/untask/properties/dependencies)
 (prefix-in links: untask/src/untask/properties/links)
 (prefix-in date: untask/src/untask/properties/date)

 untask/src/untask/user/render-list

 (prefix-in a: untask/src/attribute)
 (prefix-in term: untask/src/terminal)
 (prefix-in dt: untask/src/datetime)
 (only-in untask/src/misc try-read-line)
 untask/src/squiggle)

;; For convenience, the internal procedures in this module pass boxed state
;; around as a parameter. Error reporting is done using exceptions.

(define *state (make-parameter #f))
(define (state) (unbox (*state)))
(define (item-state) (a:get-path ((state) state.item-state)))

;; Check whether fme is a valid filter or modify expression. If it isn't, throw
;; an exception.
(define (check! fme #:filter? filter?)
  (let ((check (check-filter/modify-expression fme filter?)))
    (when (not (eq? #t check))
      (raise-user-error check))))

;; Extend a filter or modify expression with the currently enabled contexts.
(define (with-contexts fme #:filter? filter?)
  `(and ,fme
        ,((if filter? c:activated-filter c:activated-modify)
          (a:get-path ((state) state.context-state)))))

;; Find all items matching filter expression exactly.
(define (search* fe)
  (filter:search (item-state) fe))

;; Like search*, but automatically uses current contexts and returns list sorted
;; by urgency rather than set.
(define (search fe)
  (urgency:sort-items-by-urgency-descending
   (item-state)
   (search* (with-contexts fe #:filter? #t))))

(define (list! items)
  (displayln (render-listing (item-state) items)))

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
            (equal? (serialize:load-state (a:get-path ((state) state.open-file))) (state)))
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
  (displayln (render-listing-info (item-state) (search fe))))

;; TODO: Create other version of "tree" that shows the entire tree, including
;; parents/blocked items. It should have some way of highlighting the current
;; item.

(define (execute-tree! fe post-fe)
  (displayln (render-trees (item-state) (cmd:tree (state) #:filter fe #:post-filter post-fe))))

(define (execute-agenda! fe)
  (check! fe #:filter? #t)
  (displayln (render-agenda (item-state)
                            (sort (foldl (λ (item blocks)
                                           (let ((item-date (val:unwrap-date (p:get (item-state) item date:date-property))))
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
  (let-values (((item-state* item*) (i:new (item-state))))
    (set-box! (*state)
              (a:set-path ((state) state.item-state)
                          (modify:evaluate-modify-expression (with-contexts me #:filter? #f)
                                                             item-state* item*)))
    (list! (list item*))))

(define (execute-modify! fe me)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (let ((items (search fe)))
    (set-box! (*state)
              (a:update-path ((state) state.item-state)
                             (λ (item-state)
                               (modify:modify-items item-state (list->set items) me))))
    (list! items)))

(define (execute-copy! fe me)
  (define (copy-item! item)
    (let-values (((item-state* item*) (i:clone (item-state) item)))
      (set-box! (*state) (a:set-path ((state) state.item-state)
                                     (modify:evaluate-modify-expression (with-contexts me #:filter? #f)
                                                                        item-state* item*)))
      item*))
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (list! (map (λ (item)
                (copy-item! item))
              (search fe))))

(define (execute-copy-recur! fe me start end skip)
  (define start/dt (val:unwrap-date (val:evaluate-literal start)))
  (define end/dt (val:unwrap-date (val:evaluate-literal end)))
  (define items (search fe))
  (define var-result-items (list))
  (for-each (λ (item)
              (let-values (((item-state* items*) (date:copy-recur (item-state) item #:start start/dt #:end end/dt #:skip skip)))
                (set-box! (*state) (a:set-path ((state) state.item-state) item-state*))
                (for-each (λ (item*)
                            (set-box! (*state) (a:set-path ((state) state.item-state)
                                                           (modify:evaluate-modify-expression me (item-state) item*))))
                          items*)
                (set! var-result-items (append var-result-items items*))))
            items)
  (list! var-result-items))

(define (execute-remove! fe)
  (check! fe #:filter? #t)
  (set-box! (*state)
            (a:update-path ((state) state.item-state)
                           (λ (item-state)
                             (foldl (λ (item item-state)
                                      (i:remove item-state item))
                                    item-state
                                    (search fe))))))

;; Execute a command and update the state by setting the boxed state. Return
;; 'exit if the given command should cause the program to exit, and return
;; 'proceed otherwise.
(define (execute! command state-box)
  (parameterize ((*state state-box))
    (with-handlers ((exn:fail:user?
                     (λ (e) (displayln (term:render `((bold) (red) ("Error: " ,(exn-message e))))) 'proceed)))
      (match command
        (`(,fe list) (execute-list! fe) 'proceed)
        (`(add ,me) (execute-add! me) 'proceed)
        (`(,fe modify ,me) (execute-modify! fe me) 'proceed)
        (`(,fe copy ,me) (execute-copy! fe me) 'proceed)
        (`(,fe copy-recur ,me ,start ,end ,skip) (execute-copy-recur! fe me start end skip) 'proceed)
        (`(,fe remove) (execute-remove! fe) 'proceed)
        (`(,fe agenda) (execute-agenda! fe) 'proceed)
        (`(,fe tree ,rhs) (execute-tree! fe rhs) 'proceed)
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
                                           (λ> (c:toggle toggles))))
         'proceed)
        (`(context add ,name ,fe ,me)
         (check! fe #:filter? #t)
         (check! me #:filter? #f)
         (set-box! (*state) (a:update-path ((state) state.context-state)
                                           (λ> (c:register name #:filter fe #:modify me))))
         'proceed)
        (`(context remove ,name)
         (set-box! (*state) (a:update-path ((state) state.context-state)
                                           (λ> (c:remove name))))
         'proceed)
        (`(context show)
         (display-message! (format "~a" (c:available (a:get-path ((state) state.context-state)))))
         'proceed)
        (`(with-contexts ,toggles ,subcommand)
         (let ((old-context-state (a:get-path ((state) state.context-state))))
           (set-box! (*state) (a:update-path ((state) state.context-state)
                                             (λ> (c:toggle toggles))))
           (execute! subcommand (*state))
           (set-box! (*state) (a:set-path ((state) state.context-state) old-context-state))
           'proceed))
        (`(exit)
         (if (confirm-unsaved!) 'exit 'proceed))))))
