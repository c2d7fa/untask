#lang racket

(provide tree)

;; All procedures in this module accept invalid expressions as arguments, and
;; throw exceptions with a human-readable error when such arguments are
;; given. The caller is supposed to handle such errors by printing them to the
;; user.

(require untask/src/untask/core/state
         (prefix-in c: untask/src/untask/core/context)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in p: untask/src/untask/core/property)

         untask/src/untask/command/check-expression
         (prefix-in filter: untask/src/untask/command/filter)
         (prefix-in modify: untask/src/untask/command/modify)

         (prefix-in urgency: untask/src/untask/properties/urgency)
         (prefix-in depends: untask/src/untask/properties/dependencies)
         (prefix-in links: untask/src/untask/properties/links)

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
