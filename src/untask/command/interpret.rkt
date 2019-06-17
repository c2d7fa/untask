#lang racket

(provide interpret
         run!
         run-state)

(require
 )

;; Takes a command (the value returned by parse) and returns an interpretation.
;;
;; An interpretation is a Racket expression which is one of the following:
;; - (value x)
;; - (get-state (λ (state) ...))
;; - (set-state state (λ () ...))
;; - (read-file filename (λ (data) ...))
;; - (write-file filename data (λ () ...))
;; - (confirm prompt (λ (confirmed?) ...))
;;
;; The interpretation (value x) represents the simple value x. In each other
;; case, the interpretation represents some operation, whose behavior is defined
;; elsewhere, associated with a function that consumes the result of that
;; operation and returns the next operation.
(define (interpret command #:property-types property-types)
  (match command
    (`(open ,filename)
     `(get-state ,(λ (state)
     `(read-file ,filename ,(λ (file-content)
     (if (newer? file-content state)
       `(confirm "You have unsaved data. Proceed?" ,(λ (answer)
       (if answer
         `(set-state (read-state file-content))
         `())))
       `(set-state (read-state file-content))))))))))

;; Run an interpretation by writing and reading to actual files, asking the user
;; for confirmation and writing to standard output. Update the state by setting
;; the boxed state.
(define (run! interpretation state-box #:property-types property-types)
  'undefined)

;; Run an interpretation given an initial state. Returns the state after
;; evaluating the interpretation. Does not allow any side-effects.
(define (run interpretation init-state #:property-types property-types)
  'undefined)
