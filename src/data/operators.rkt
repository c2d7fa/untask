#lang racket

(provide (all-defined-out))

(require (only-in "../util.rkt" thread-first))

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
  (hash-ref opdefs (list* name object-type filter-context?)))

(define (type-of value)
  (cond
    ((string? value) 'string)
    ((number? value) 'item)
    ((set? value) 'set)
    ((boolean? value) 'boolean)
    ((and (pair? value) (eq? 'number (car value))) 'number)
    (else 'any)))

;; TODO: This should be allowed to access item-data (for resolving
;; properties on items mentioned.)
(define (evaluate-operator-expression opdefs expression (filter-context? #f))
  (match expression
    (`(,object ,operator ,arguments ...)
     (apply operator-eval
            (operator-definitions-find opdefs operator (type-of object) filter-context?)
            object
            arguments))))

;; TODO: This should definitely be somewhere else
(define (number-value x)
  (cdr x))
(define (make-number x)
  (cons 'number x))

;;; COMMON OPERATORS
;; (TODO: Move this somewhere else.)

(define op-string-suffix
  (create-operator #:name '>
                   #:object 'string
                   #:arguments '(string)
                   #:return 'string
                   #:body (λ (str sfx) (string-append str sfx))))
(define op-string-prefix
  (create-operator #:name '<
                   #:object 'string
                   #:arguments '(string)
                   #:return 'string
                   #:body (λ (str pfx) (string-append pfx pfx))))

(define op-number-add
  (create-operator #:name '+
                   #:object 'number
                   #:arguments '(number)
                   #:return 'number
                   #:body (λ (x y) (make-number (+ (number-value x) (number-value y))))))
(define op-number-subtract
  (create-operator #:name '-
                   #:object 'number
                   #:arguments '(number)
                   #:return 'number
                   #:body (λ (x y) (make-number (- (number-value x) (number-value y))))))

(define op-set-add
  (create-operator #:name '+
                   #:object 'set
                   #:arguments '(any)
                   #:return 'set
                   #:body (λ (xs x) (set-add xs x))))
(define op-set-remove
  (create-operator #:name '-
                   #:object 'set
                   #:arguments '(any)
                   #:return 'set
                   #:body (λ (xs x) (set-remove xs x))))

(define op-string-match?
  (create-operator #:name '/
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str sst) (string-contains? str sst))))
(define op-string-prefix?
  (create-operator #:name '<
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str prf) (string-prefix? str prf))))
(define op-string-suffix?
  (create-operator #:name '>
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str sfx) (string-suffix? str sfx))))

(define op-set-contains?
  (create-operator #:name '+
                   #:object 'set
                   #:arguments '(any)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (xs x) (set-member? xs x))))
(define op-set-doesnt-contain?
  (create-operator #:name '-
                   #:object 'set
                   #:arguments '(any)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (xs x) (not (set-member? xs x)))))

(define op-number-greater-than?
  (create-operator #:name '>
                   #:object 'number
                   #:arguments '(number)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (x y) (> (number-value x) (number-value y)))))
(define op-number-less-than?
  (create-operator #:name '<
                   #:object 'number
                   #:arguments '(number)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (x y) (< (number-value x) (number-value y)))))

;;

(define common-operators
  (thread-first operator-definitions-empty
    (operator-definitions-add op-string-suffix)
    (operator-definitions-add op-string-prefix)
    (operator-definitions-add op-number-add)
    (operator-definitions-add op-number-subtract)
    (operator-definitions-add op-set-add)
    (operator-definitions-add op-set-remove)

    (operator-definitions-add op-string-match?)
    (operator-definitions-add op-string-prefix?)
    (operator-definitions-add op-string-suffix?)
    (operator-definitions-add op-set-contains?)
    (operator-definitions-add op-set-doesnt-contain?)
    (operator-definitions-add op-number-greater-than?)
    (operator-definitions-add op-number-less-than?)
    ))
