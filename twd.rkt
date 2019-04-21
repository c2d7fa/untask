#lang racket

(require racket/undefined)

(require racket/set)

(define (set-filter pred set)
  (list->set (filter pred (set->list set))))

;;; STATE

(struct state (next-id descriptions statuses tag-sets) #:transparent)

;; todo: i bet there are library functions for this:
(define (state-set-next-id state- next-id)
  (state next-id (state-descriptions state-) (state-statuses state-) (state-tag-sets state-)))
(define (state-set-descriptions state- descriptions)
  (state (state-next-id state-) descriptions (state-statuses state-) (state-tag-sets state-)))
(define (state-set-statuses state- statuses)
  (state (state-next-id state-) (state-descriptions state-) statuses (state-tag-sets state-)))
(define (state-set-tag-sets state- tag-sets)
  (state (state-next-id state-) (state-descriptions state-) (state-statuses state-) tag-sets))
(define ((make-update get set) struct f)
  (set struct (f (get struct))))
(define state-update-next-id (make-update state-next-id state-set-next-id))
(define state-update-descriptions (make-update state-descriptions state-set-descriptions))
(define state-update-statuses (make-update state-statuses state-set-statuses))
(define state-update-tag-sets (make-update state-tag-sets state-set-tag-sets))

(define empty-state (state 0 (hash) (hash) (hash)))

;;; STATE QUERIES

;; todo: instead return special id set type
(define (all-ids state)
  (list->set
   (append
    (dict-keys (state-descriptions state))
    (dict-keys (state-statuses state)))))

(define (get-status id state)
  (hash-ref (state-statuses state) id 'todo))

(define (get-description id state)
  (hash-ref (state-descriptions state) id "[no description]"))

(define (get-tags id state)
  (hash-ref (state-tag-sets state) id (set)))

(define (is-active id state)
  (equal? (get-status id state) 'todo))

(define (all-active-ids state)
  (set-filter (λ (id) (is-active id state)) (all-ids state)))
(define (all-uncompleted-ids state)
  (set-filter (λ (id) (not (equal? (get-status id state) 'done))) (all-ids state)))

(define (has-tag? id tag state)
  (set-member? (get-tags id state) tag))

(define (search-ids-matching-query query state)
  (define (matches-query? query id state)
    (cond
      ((equal? query 'active) (is-active id state))
      ((equal? (car query) 'has-tag) (has-tag? id (cadr query) state))
      ((equal? (car query) 'not) (not (matches-query? (cadr query) id state)))
      ((equal? (car query) 'and) (andmap (λ (q) (matches-query? q id state)) (cdr query)))
      (else (error (format "unknwon query type: ~s" query)))))
  (set-filter (λ (id) (matches-query? query id state)) (all-ids state)))

;;; STATE UPDATES

(define (set-tags id new-tags state)
  (state-update-tag-sets state (λ (tag-sets) (hash-set tag-sets id new-tags))))
(define (update-tags id f state)
  (set-tags id (f (get-tags id state)) state))
(define (add-tag id new-tag state)
  (update-tags id (λ (tags) (set-add tags new-tag)) state))
(define (remove-tag id old-tag state)
  (update-tags id (λ (tags) (set-remove tags old-tag)) state))

(define (set-status id new-status state)
  (state-update-statuses state (λ (statuses) (hash-set statuses id new-status))))
(define (set-status-done id state) (set-status id 'done state))
(define (set-status-todo id state) (set-status id 'todo state))
(define (set-status-paused id state) (set-status id 'paused state))

(define (set-description id state new-description)
  (state-update-descriptions state (λ (descriptions) (hash-set descriptions id new-description))))

(define (increment-id state)
  (define next-id (state-next-id state))
  (define new-state (state-update-next-id state add1))
  (values next-id new-state))

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
(execute '(search (not (and active (has-tag "health")))))





