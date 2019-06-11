#lang racket

(provide (all-defined-out))

(require (only-in "../../misc.rkt" thread-first)
         (prefix-in val: "value.rkt"))

(define (create-operator #:name name
                         #:object object-type
                         #:filter? (filter-context #f)
                         #:body body-procedure
                         #:check-types (check-types (λ (object-type argument-types) #t))
                         )
  (list name object-type filter-context body-procedure check-types))

(define (operator-name op) (list-ref op 0))
(define (operator-object-type op) (list-ref op 1))
(define (operator-filter-context? op) (list-ref op 2))
(define (operator-eval op object . args)
  (apply (list-ref op 3) object args))
(define (operator-check-types op)
  (list-ref op 4))

;;

(define operator-definitions-empty (hash))

(define (operator-definitions-add opdefs operator)
  (hash-set opdefs (list*
                    (operator-name operator)
                    (operator-object-type operator)
                    (operator-filter-context? operator))
            operator))

(define (operator-definitions-find opdefs name object-type (filter-context? #f))
  (let ((object-type* (if (list? object-type) (car object-type) object-type)))
    (hash-ref opdefs (list* name object-type* filter-context?) #f)))

(define (evaluate-operator-expression opdefs expression (filter-context? #f))
  (match expression
    (`(,object ,operator ,argument-literal-exprs ...)
     (let ((result (apply operator-eval
                          (operator-definitions-find opdefs operator (val:get-type object) filter-context?)
                          object
                          (map val:evaluate-literal argument-literal-exprs))))
       (if filter-context? (val:unwrap-boolean result) result)))))

;; Returns #t if operator can be used with an object of type object-type and
;; argument-types as arguments. Otherwise, returns a human-readable string
;; describing the reason why this is not a valid cominations of object and
;; argument types.
(define (check-types operator #:object-type object-type #:argument-types argument-types)
  ((operator-check-types operator) object-type argument-types))
