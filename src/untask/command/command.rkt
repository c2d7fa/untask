#lang racket

(provide (rename-out (cmd:list list))
         tree
         agenda
         add
         modify
         copy)

;; All procedures in this module accept invalid expressions as arguments, and
;; throw exceptions with a human-readable error when such arguments are
;; given. The caller is supposed to handle such errors by printing them to the
;; user.

(require untask/src/untask/core/state
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in c: untask/src/untask/core/context)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in p: untask/src/untask/core/property)

         untask/src/untask/command/check-expression
         (prefix-in filter: untask/src/untask/command/filter)
         (prefix-in modify: untask/src/untask/command/modify)

         (prefix-in urgency: untask/src/untask/properties/urgency)
         (prefix-in depends: untask/src/untask/properties/dependencies)
         (prefix-in links: untask/src/untask/properties/links)
         (prefix-in date: untask/src/untask/properties/date)

         untask/src/squiggle
         (prefix-in dt: untask/src/datetime)
         (prefix-in a: untask/src/attribute))

;; Check whether fme is a valid filter or modify expression. If it isn't, throw
;; an exception.
(define (check! fme #:filter? filter?)
  (let ((check (check-filter/modify-expression fme filter?)))
    (when (not (eq? #t check))
      (raise-user-error check))))   ; TODO: Throw a more specific exception here.

;; Extend a filter or modify expression with the currently enabled contexts.
(define (with-contexts state fme #:filter? filter?)
  `(and ,fme
        ,((if filter? c:activated-filter c:activated-modify)
          (a:get-path (state state.context-state)))))

;; Find all items matching filter expression exactly.
(define (search* state fe)
  (filter:search (a:get-path (state state.item-state)) fe))

;; Like search*, but automatically uses current contexts and returns list sorted
;; by urgency rather than set.
(define (search state fe)
  (urgency:sort-items-by-urgency-descending
   (a:get-path (state state.item-state))
   (search* state (with-contexts state fe #:filter? #t))))

;;;

(define (cmd:list state #:filter fe)
  (check! fe #:filter? #t)
  (search state fe))

;; Returns a list of tree views, which can be rendered with 'render-trees'.
(define (tree state #:filter fe #:post-filter post-fe)
  (check! fe #:filter? #t)
  (check! post-fe #:filter? #t)
  (define item-state (a:get-path (state state.item-state)))
  ;; TODO: Distinguish between children and dependencies in output
  (define seen (list))
  (define (build-tree item-state item)
    ;; TODO: This code is ugly.
    (define (build-tree* item)
      (cons item (map (λ (i)
                        (if (member i seen)
                            (let ((has-children? (not (and (null? (set->list (v:unwrap-set (p:get item-state (v:unwrap-item i) depends:depends-property))))
                                                           (null? (set->list (v:unwrap-set (p:get item-state (v:unwrap-item i) links:children-property))))))))
                              `(,(v:unwrap-item i) ,@(if has-children? '(()) '())))
                            (begin (set! seen (cons i seen))
                                   (build-tree* (v:unwrap-item i)))))
                      (filter (λ (i)
                                (filter:evaluate-filter-expression post-fe item-state (v:unwrap-item i)))
                              (append (set->list (v:unwrap-set (p:get item-state item depends:depends-property)))
                                      (set->list (v:unwrap-set (p:get item-state item links:children-property))))))))
    (build-tree* item))
  (values (map (λ (item)
                 (build-tree item-state item))
               (search state fe))))

;; Returns an agenda view, which can be rendered with 'render-agenda'.
(define (agenda state #:filter fe)
  (define item-state (a:get-path (state state.item-state)))
  (check! fe #:filter? #t)
  (sort (foldl (λ (item blocks)
                 (let ((item-date (v:unwrap-date (p:get item-state item date:date-property))))
                   (if (assoc item-date blocks dt:same-date?)
                       (map (λ (block)
                              (if (dt:same-date? (car block) item-date)
                                  `(,@block ,item)
                                  block))
                            blocks)
                       `(,@blocks (,item-date ,item)))))
               (list)
               (search state `(and (not (date : #f)) ,fe)))
       #:key car dt:date-before?))

;; Returns updated state and new item.
(define (add state #:modify me)
  (check! me #:filter? #f)
  (define-values (item-state* item*) (i:new (a:get-path (state state.item-state))))
  (define state*
    (a:set-path (state state.item-state)
      (modify:evaluate-modify-expression (with-contexts state me #:filter? #f)
                                         item-state* item*)))
  (values state* item*))

;; Returns updated state and modified items.
(define (modify state #:filter fe #:modify me)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (define items (search state fe))
  (values (a:update-path (state state.item-state)
                         (λ> (modify:modify-items (list->set items) me)))
          items))

;; Returns updated state and newly created items.
(define (copy state #:filter fe #:modify me)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (define items (search state fe))
  (define var-items* (list))
  (define state*
    (a:update-path (state state.item-state)
      (λ (item-state)
        (foldl (λ (item item-state)
                 (let-values (((item-state* item*) (i:clone item-state item)))
                   (set! var-items* (append var-items* (list item*)))
                   (modify:evaluate-modify-expression (with-contexts state me #:filter? #f)
                                                      item-state* item*)))
               item-state
               items))))
  (values state* var-items*))
