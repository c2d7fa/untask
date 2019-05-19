#lang racket

(provide (all-defined-out))

(require (only-in "../util.rkt" thread-first)
         (prefix-in val: "values.rkt"))

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
  (car value))  ;; TODO: Temporary hack. Should probably be handled by values module.

;; TODO: This should be allowed to access item-data (for resolving
;; properties on items mentioned.)
(define (evaluate-operator-expression opdefs expression (filter-context? #f))
  (match expression
    (`(,object ,operator ,argument-literal-exprs ...)
     (apply operator-eval
            (operator-definitions-find opdefs operator (type-of object) filter-context?)
            object
            (map val:evaluate-literal argument-literal-exprs)))))

;;; COMMON OPERATORS
;; (TODO: Move this somewhere else.)

(define op-string-suffix
  (create-operator #:name '>
                   #:object 'string
                   #:arguments '(string)
                   #:return 'string
                   #:body (λ (str sfx) (val:make-string (string-append (val:unwrap-string str) (val:unwrap-string sfx))))))
(define op-string-prefix
  (create-operator #:name '<
                   #:object 'string
                   #:arguments '(string)
                   #:return 'string
                   #:body (λ (str pfx) (val:make-string (string-append (val:unwrap-string pfx) (val:unwrap-string pfx))))))

(define op-number-add
  (create-operator #:name '+
                   #:object 'number
                   #:arguments '(number)
                   #:return 'number
                   #:body (λ (x y) (val:make-number (+ (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-subtract
  (create-operator #:name '-
                   #:object 'number
                   #:arguments '(number)
                   #:return 'number
                   #:body (λ (x y) (val:make-number (- (val:unwrap-number x) (val:unwrap-number y))))))

(define op-set-add
  (create-operator #:name '+
                   #:object 'set
                   #:arguments '(any)
                   #:return 'set
                   #:body (λ (xs x) (val:make-set (set-add (val:unwrap-set xs) x)))))
(define op-set-remove
  (create-operator #:name '-
                   #:object 'set
                   #:arguments '(any)
                   #:return 'set
                   #:body (λ (xs x) (val:make-set (set-remove (val:unwrap-set xs) x)))))

(define op-string-match?
  (create-operator #:name '/
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str sst) (val:make-boolean (string-contains? (val:unwrap-string str) (val:unwrap-string sst))))))
(define op-string-prefix?
  (create-operator #:name '<
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str prf) (val:make-boolean (string-prefix? (val:unwrap-string str) (val:unwrap-string prf))))))
(define op-string-suffix?
  (create-operator #:name '>
                   #:object 'string
                   #:arguments '(string)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (str sfx) (val:make-boolean (string-suffix? (val:unwrap-string str) (val:unwrap-string sfx))))))

(define op-set-contains?
  (create-operator #:name '+
                   #:object 'set
                   #:arguments '(any)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (xs x) (val:make-boolean (set-member? (val:unwrap-set xs) x)))))
(define op-set-doesnt-contain?
  (create-operator #:name '-
                   #:object 'set
                   #:arguments '(any)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (xs x) (val:make-boolean (not (set-member? (val:unwrap-set xs) x))))))

(define op-number-greater-than?
  (create-operator #:name '>
                   #:object 'number
                   #:arguments '(number)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (x y) (val:make-boolean (> (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-less-than?
  (create-operator #:name '<
                   #:object 'number
                   #:arguments '(number)
                   #:return 'boolean
                   #:filter? #t
                   #:body (λ (x y) (val:make-boolean (< (val:unwrap-number x) (val:unwrap-number y))))))

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
