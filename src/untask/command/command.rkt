#lang racket

(provide (rename-out (cmd:list list))
         tree
         agenda
         add
         modify
         copy
         copy-recur
         remove)

;; All procedures in this module accept invalid expressions as arguments, and
;; throw exceptions with a human-readable error when such arguments are
;; given. The caller is supposed to handle such errors by printing them to the
;; user.

(require "../../untask/core/state.rkt"
         (prefix-in i: "../../untask/core/item.rkt")
         (prefix-in c: "../../untask/core/context.rkt")
         (prefix-in v: "../../untask/core/value.rkt")
         (prefix-in p: "../../untask/core/property.rkt")

         "../../untask/command/check-expression.rkt"
         (prefix-in filter: "../../untask/command/filter.rkt")
         (prefix-in modify: "../../untask/command/modify.rkt")

         (prefix-in bp: "../../untask/properties/builtin.rkt")
         (prefix-in urgency: "../../untask/properties/urgency.rkt")
         (prefix-in depends: "../../untask/properties/dependencies.rkt")
         (prefix-in links: "../../untask/properties/links.rkt")
         (prefix-in date: "../../untask/properties/date.rkt")
         (prefix-in order: "../../untask/properties/order.rkt")

         "../../squiggle.rkt"
         (prefix-in dt: "../../datetime.rkt")
         (prefix-in a: "../../attribute.rkt"))

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
  (set->list (filter:search (a:get-path (state state.item-state)) fe)))

;; Like search*, but automatically uses current contexts.
(define (search state fe)
  (search* state (with-contexts state fe #:filter? #t)))

;; Like search, but sorts by urgency. Note: This is currently very slow for
;; largeish sets. This is because calculating the urgency is slow, which is
;; because calculating blocked items is slow, which is because it looks at the
;; properties of every item in the database each time.
(define (search/urgency state fe)
  (urgency:sort-items-by-urgency-descending (a:get-path (state state.item-state)) (search state fe)))

;;;

(define (cmd:list state #:filter fe)
  (check! fe #:filter? #t)
  (search/urgency state fe))

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
               (search/urgency state fe))))

;; Returns an agenda view, which can be rendered with 'render-agenda'.
(define (agenda state #:filter fe)
  (define item-state (a:get-path (state state.item-state)))
  (check! fe #:filter? #t)
  (sort (map (λ (block)
               (define (sort-items items)
                 (sort items
                       (λ (item1 item2)
                         (let ((order1 (p:get item-state item1 order:order-property))
                               (order2 (p:get item-state item2 order:order-property)))
                           (cond
                             ((and order1 order2) (< (v:unwrap-number order1) (v:unwrap-number order2)))
                             (order1 #t)
                             (order2 #f)
                             (else (>= (v:unwrap-number (p:get item-state item1 urgency:urgency-property))
                                       (v:unwrap-number (p:get item-state item2 urgency:urgency-property)))))))))
               `(,(car block) ,@(sort-items (cdr block))))
             (foldl (λ (item blocks)
                      (let ((item-date (v:unwrap-date (p:get item-state item date:date-property))))
                        (if (assoc item-date blocks dt:same-date?)
                            (map (λ (block)
                                   (if (dt:same-date? (car block) item-date)
                                       `(,@block ,item)
                                       block))
                                 blocks)
                            `(,@blocks (,item-date ,item)))))
                    (list)
                    (search state `(and (not (date : #f)) ,fe))))
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
                 (let-values (((item-state* item*) (bp:clone item-state item)))
                   (set! var-items* (append var-items* (list item*)))
                   (modify:evaluate-modify-expression (with-contexts state me #:filter? #f)
                                                      item-state* item*)))
               item-state
               items))))
  (values state* var-items*))

;; Returns updated state and newly created items.
(define (copy-recur state #:filter fe #:modify me #:start start #:end end #:skip skip)
  (check! fe #:filter? #t)
  (check! me #:filter? #f)
  (define start/dt (v:unwrap-date (v:evaluate-literal start)))
  (define end/dt (v:unwrap-date (v:evaluate-literal end)))
  (define items (search state fe))
  (define var-result-items* (list))
  (define var-result-state* state)
  (for-each (λ (item)
              (let-values (((item-state* items*) (date:copy-recur (a:get-path (var-result-state* state.item-state)) item #:start start/dt #:end end/dt #:skip skip)))
                (set! var-result-state* (a:set-path (var-result-state* state.item-state) item-state*))
                (for-each (λ (item*)
                            (set! var-result-state* (a:set-path (var-result-state* state.item-state)
                                                                (modify:evaluate-modify-expression me (a:get-path (var-result-state* state.item-state)) item*))))
                          items*)
                (set! var-result-items* (append var-result-items* items*))))
            items)
  (values var-result-state*
          var-result-items*))

;; Returns updated state.
(define (remove state #:filter fe)
  (check! fe #:filter? #t)
  (a:update-path (state state.item-state)
                 (λ (item-state)
                   (foldl (λ (item item-state)
                            (i:remove item-state item))
                          item-state
                          (search state fe)))))
