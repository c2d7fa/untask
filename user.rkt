#lang racket

(require "state.rkt"
         "item.rkt"
         "filtering.rkt"
         "urgency.rkt"
         (only-in "util.rkt" set-filter))

(provide (all-defined-out))

;; Helper procedures

(define (list-items items)
  (define (format-tags item)
    (string-join (set->list (item-tags item)) " "))
  (define (format-active item)
    (if (active? item) "active" "inactive"))
  (for ((item (sort-items-by-urgency-descending items)))
    (printf "~a. ~a (~a, ~a) [~a] - ~a~n"
            (item-id item)
            (item-description item)
            (item-status item)
            (format-active item)
            (format-tags item)
            (urgency item))))

;; User interaction procedues

(define (user-add-task !state description)
  (define-values (next-id state-with-new-item) (increment-id (unbox !state)))
  (set-box! !state (set-description next-id state-with-new-item description)))

(define (user-add-tag !state task-id tag-label)
  (set-box! !state (add-tag task-id tag-label (unbox !state))))

(define (user-remove-tag !state task-id tag-label)
  (set-box! !state (remove-tag task-id tag-label (unbox !state))))

(define (user-mark-done !state task-id)
  (set-box! !state (set-status-done task-id (unbox !state))))

(define (user-mark-todo !state task-id)
  (set-box! !state (set-status-todo task-id (unbox !state))))

(define (user-mark-paused !state task-id)
  (set-box! !state (set-status-paused task-id (unbox !state))))

(define (user-list-active-tasks !state)
  (list-items (filter-active (all-items (unbox !state)))))

(define (user-list-all-tasks !state)
  (list-items (all-items (unbox !state))))

(define (user-list-uncompleted-tasks !state)
  (list-items (filter-uncompleted (all-items (unbox !state)))))

(define (user-list-search-tasks !state query)
  (list-items (filter-matching-query query (all-items (unbox !state)))))

(define (user-set-description !state id description)
  (set-box! !state (set-description id (unbox !state) description)))

(define (user-set-base-urgency !state id base-urgency)
  (set-box! !state (set-base-urgency id (unbox !state) base-urgency)))

;; Command Line

(define (execute !state command)
  (match command
    (`(add ,desc) (user-add-task !state desc))
    (`(done ,id) (user-mark-done !state id))
    (`(todo ,id) (user-mark-todo !state id))
    (`(pause ,id) (user-mark-paused !state id))
    (`(list) (user-list-active-tasks !state))
    (`(search ,query) (user-list-search-tasks !state query))
    (`(list-all) (user-list-all-tasks !state))
    (`(list-uncompleted) (user-list-uncompleted-tasks !state))
    (`(desc ,id ,desc) (user-set-description !state id desc))
    (`(urg ,id ,urg) (user-set-base-urgency !state id urg))
    (`(tag ,id ,tag) (user-add-tag !state id tag))
    (`(untag ,id ,tag) (user-remove-tag !state id tag))))

(define (start-command-line-loop !state)
  (let ((command (read)))
    (if (equal? command '(quit))
        (void)
        (begin
          (execute !state command)
          (start-command-line-loop)))))
