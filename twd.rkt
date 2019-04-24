#lang racket

(require "state.rkt")

;;; USER INTERACTION

(define current-state empty-state)

(define (user-add-task description)
  (define-values (next-id new-state1) (increment-id current-state))
  (define new-state2 (set-description next-id new-state1 description))
  (set! current-state new-state2)
  (void))

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

(define (execute command)
  (let ((args (cdr command)))
    (case (car command)
      ((add) (user-add-task (car args)))
      ((done) (user-mark-done (car args)))
      ((todo) (user-mark-todo (car args)))
      ((pause) (user-mark-paused (car args)))
      ((list) (user-list-active-tasks))
      ((search) (user-list-search-tasks (car args)))
      ((list-all) (user-list-all-tasks))
      ((list-uncompleted) (user-list-uncompleted-tasks))
      ((desc) (user-set-description (car args) (cadr args)))
      ((tag) (user-add-tag (car args) (cadr args)))
      ((untag) (user-remove-tag (car args) (cadr args)))
      )))

(define (start-command-line-loop)
  (let ((command (read)))
    (if (equal? command '(quit))
        (void)
        (begin
          (execute command)
          (start-command-line-loop)))))

;; testing setup:
(execute '(add "eat food"))
(execute '(tag 0 "health"))
(execute '(add "buy milk"))
(execute '(tag 1 "shopping"))
(execute '(tag 1 "outside"))
(execute '(add "drink milk"))
(execute '(tag 2 "health"))
(execute '(add "go for a walk"))
(execute '(tag 3 "outside"))
(execute '(tag 3 "health"))
(execute '(done 1))
(execute '(pause 0))
(displayln "  all:")
(execute '(list-all))
(displayln "  search:")
(execute '(search (or (has-tag "outside") (not (and active (has-tag "health"))))))





