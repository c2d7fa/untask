#lang racket

(provide (all-defined-out))

(require (only-in "../../misc.rkt" thread-first)
         (prefix-in val: "value.rkt"))

(define (create-operator #:name name
                         #:object object-type
                         #:arguments (argument-types '())
                         #:return return-type
                         #:filter? (filter-context #f)
                         #:body body-procedure
                         )
  (list* name object-type argument-types return-type filter-context body-procedure))

(define (operator-name op) (car op))
(define (operator-object-type op) (car (cdr op)))
(define (operator-argument-types op) (caddr op))
(define (operator-eval op object . args)
  (apply (cdr (cdr (cdr (cdr (cdr op))))) object args))
(define (operator-return-type op)
  (car (cdr (cdr (cdr op)))))
(define (operator-filter-context? op)
  (car (cdr (cdr (cdr (cdr op))))))

;;

(define operator-definitions-empty (hash))

(define (operator-definitions-add opdefs operator)
  (hash-set opdefs (list*
                    (operator-name operator)
                    (operator-object-type operator)
                    (operator-filter-context? operator))
            operator))

(define (operator-definitions-find opdefs name object-type (filter-context? #f))
  (hash-ref opdefs (list* name object-type filter-context?) #f))

(define (evaluate-operator-expression opdefs expression (filter-context? #f))
  (match expression
    (`(,object ,operator ,argument-literal-exprs ...)
     (let ((result (apply operator-eval
                          (operator-definitions-find opdefs operator (val:get-type object) filter-context?)
                          object
                          (map val:evaluate-literal argument-literal-exprs))))
       (if filter-context? (val:unwrap-boolean result) result)))))
