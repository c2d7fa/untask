#lang racket

(require "state.rkt"
         "item.rkt"
         "filtering.rkt")

;;

(define (user-add-task description)
  (define-values (next-id state-with-new-item) (increment-id current-state))
  (set! current-state (set-description next-id state-with-new-item description)))

(define (user-add-tag task-id tag-label)
  (set! current-state (add-tag task-id tag-label current-state)))

(define (user-remove-tag task-id tag-label)
  (set! current-state (remove-tag task-id tag-label current-state)))

(define (user-mark-done task-id)
  (set! current-state (set-status-done task-id current-state))
  (void))
(define (user-mark-todo task-id)
  (set! current-state (set-status-todo task-id current-state)))
(define (user-mark-paused task-id)
  (set! current-state (set-status-paused task-id current-state)))

(define (user-list-ids ids)
  (define (format-tags tag-set)
    (string-join (set->list tag-set) " "))
  (for ((id ids))
    (define active-string
      (if (is-active id current-state) "active" "inactive"))
    (printf "~a. ~a (~a, ~a) [~a]~n" id (get-description id current-state) (get-status id current-state) active-string (format-tags (get-tags id current-state))))
  (void))

(define (user-list-active-tasks)
  (user-list-ids (all-active-ids current-state)))
(define (user-list-all-tasks)
  (user-list-ids (all-ids current-state)))
(define (user-list-uncompleted-tasks)
  (user-list-ids (all-uncompleted-ids current-state)))
(define (user-list-search-tasks query)
  (user-list-ids (search-ids-matching-query query current-state)))

(define (user-set-description id new-description)
  (set! current-state (set-description id current-state new-description)))

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
    (`(tag ,id ,tag) (user-add-tag !state id tag))
    (`(untag ,id ,tag) (user-remove-tag !state id tag))))

(define (start-command-line-loop !state)
  (let ((command (read)))
    (if (equal? command '(quit))
        (void)
        (begin
          (execute !state command)
          (start-command-line-loop)))))

;; TESTING

;;

(define !state (box empty-state))

;; testing setup:
(execute !state '(add "eat food"))
(execute !state '(tag 0 "health"))
(execute !state '(add "buy milk"))
(execute !state '(tag 1 "shopping"))
(execute !state '(tag 1 "outside"))
(execute !state '(add "drink milk"))
(execute !state '(tag 2 "health"))
(execute !state '(add "go for a walk"))
(execute !state '(tag 3 "outside"))
(execute !state '(tag 3 "health"))
(execute !state '(done 1))
(execute !state '(pause 0))
(displayln "  all:")
(execute !state '(list-all))
(displayln "  search:")
(execute !state '(search (or (has-tag "outside") (not (and active (has-tag "health"))))))




