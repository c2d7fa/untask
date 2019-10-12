#lang racket

(provide state?
         empty-state

         available-names
         available?
         register
         remove

         activate
         deactivate
         deactivate-all

         activated-names
         activated-filter
         activated-modify
         toggle)

(require (prefix-in rkt: racket)
         (prefix-in a: "../../attribute.rkt")
         "../../squiggle.rkt")

;; A context consists of a filter and modify expression that are automatically
;; applied to the user's inputs when that context is active.
(a:define-species context (filter modify))

;; The program must keep track of all known contexts and their names as well as
;; the actiated contexts.
(a:define-species state (available activated))
(define empty-state (state #:available (hash) #:activated (list)))

;; Returns a list of the names of all available contexts.
(define (available-names state)
  (hash-keys (a:get state state.available)))

;; Returns #t if the given context is availabe, #f otherwise.
(define (available? state name)
  (hash-has-key? (a:get-path (state state.available)) name))

;; Activate a context if it is available; otherwise, do nothing.
(define (activate state name)
  (if (available? state name)
      (a:update-path (state state.activated)
                     (λ> (append (list name))))
      state))

;; Deactivate a context.
;;
;; If no such context is active, do nothing.
(define (deactivate state name)
  (a:update-path (state state.activated)
                 (λ>> (rkt:remove name))))

;; Deactivate all contexts.
(define (deactivate-all state)
  (a:set-path (state state.activated) (list)))

;; Create a new context or update an existing cotnext in the collection of
;; available contexts.
(define (register state name #:filter (filter-expression '()) #:modify (modify-expression '()))
  (a:set-path (state state.available (a:hash. name))
              (context #:filter filter-expression #:modify modify-expression)))

;; Remove an existing context by name from the collection of available contexts.
;;
;; This also deactivates the context if it is currently active. If the given
;; context is not available, this function does nothing.
(define (remove state name)
  (~> state
      (a:update state.available (λ> (hash-remove name)))
      (deactivate name)))

;; Returns the filter expression corresponding to the given context. If no such
;; context is available, throw an error.
(define (filter-expression state name)
  (or (a:get-path (state state.available (a:hash. name) context.filter))
      (error (format "No such context ~a - state is ~s" name state))))

;; Returns the modify expression corresponding to the given context. If no such
;; context is available, throw an error.
(define (modify-expression state name)
  (or (a:get-path (state state.available (a:hash. name) context.modify))
      (error (format "No such context ~a - state is ~s" name state))))

;; Returns the list of names of the currently activated contexts.
(define (activated-names state)
  (a:get-path (state state.activated)))

;; Return the filter expression that results from the currently activated contexts.
(define (activated-filter state)
  `(and ,@(map (λ>> (filter-expression state)) (a:get-path (state state.activated)))))

;; Return the modify expression that results from the currently activated contexts.
(define (activated-modify state)
  `(and ,@(map (λ>> (modify-expression state)) (a:get-path (state state.activated)))))

;; Take a list containing the forms (on ctx), (off ctx) and (reset). Modify
;; state by activating and deactivating contexts and resetting the activated
;; contexts.
(define (toggle state toggles)
  (foldl (λ (tog state)
           (match tog
             (`(on ,name) (activate state name))
             (`(off ,name) (deactivate state name))
             (`(reset) (deactivate-all state))))
         state
         toggles))
