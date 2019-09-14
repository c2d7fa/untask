#lang racket

(provide (all-defined-out))

;;; Operators
;;;
;;; In a command such as "description/refactor modify tags+refactor", "/" and
;;; "+" are operators.
;;;
;;; There are two kinds of operators: filter operators and modify operators.
;;;
;;; A filter operator is a function of two arguments that returns whether the
;;; argument "matches" the first argument. For example, in "tags+hello", the
;;; plus operator matches if the value of "tags" contains the string
;;; "hello". Note that the operators themselves do not care about properties:
;;; for a given item, this operator receives the value of the "tags" property
;;; and the string value "hello".
;;;
;;; Modify operators are similar to filter operators, but instead of simply
;;; returning match or no-match, they can return a new value. When modify
;;; operators are used in a command, the result of the operator is usually used
;;; as the new value of some property. For example, in "modify tags+hello", the
;;; value of tags is replaced with the result of callling "+" with the current
;;; value of tags and the string value "hello".
;;;
;;; Operators are asymmetrical: The first argument to an operator is called the
;;; object. The meaning of a given operator depends entirely on the type of the
;;; object. Thus, "+" has a completely different meaning depending on whether
;;; its first argument is a string or a set. An operator can also (and usually
;;; will) impose constraints on its other arguments, but these are not taken
;;; into account when looking up which operator to call.

(require (only-in "../../misc.rkt" thread-first)
         (prefix-in val: "value.rkt"))

(define (create-operator #:name name
                         #:object object-type
                         #:filter? (filter-context #f)
                         #:body body-procedure
                         #:check-types (check-types (λ (object-type argument-type) #t)))
  (list name object-type filter-context body-procedure check-types))

(define (operator-name op) (list-ref op 0))
(define (operator-object-type op) (list-ref op 1))
(define (operator-filter-context? op) (list-ref op 2))
(define (operator-eval op object argument) ((list-ref op 3) object argument))
(define (operator-check-types op) (list-ref op 4))

;;

(define operator-definitions-empty (hash))

(define (operator-definitions-add opdefs operator)
  (hash-set opdefs (list*
                    (operator-name operator)
                    (operator-object-type operator)
                    (operator-filter-context? operator))
            operator))

(define (operator-definitions-find opdefs name object-type (filter-context? #f))
  (let ((object-type* (match object-type
                        (`(set ,t) 'set)
                        (`(opt ,t) t)
                        (else object-type))))
    (or (hash-ref opdefs (list* name object-type* filter-context?) #f)
        (hash-ref opdefs (list* name 'any filter-context?) #f))))

(define (evaluate-operator-expression opdefs expression (filter-context? #f))
  (match expression
    (`(,object ,operator ,argument)
     ;; Note: object will be #f when it has type (opt t) and operator works on
     ;; type t. In this case, we return #f in filter context, which means not to
     ;; include an item.
     (if (and filter-context? (not object)) #f
         (operator-eval (operator-definitions-find opdefs operator (val:get-type object) filter-context?)
                        object
                        argument)))))

;; Returns #t if operator can be used with an object of type object-type and
;; argument-type as the argument. Otherwise, returns a human-readable string
;; describing the reason why this is not a valid cominations of object and
;; argument types.
(define (check-types operator #:object-type object-type #:argument-type argument-type)
  ((operator-check-types operator) object-type argument-type))
