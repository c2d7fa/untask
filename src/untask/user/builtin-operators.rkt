#lang racket

(provide builtin-operators)

(require "../core/operator.rkt"
         (prefix-in val: "../core/value.rkt")
         (prefix-in dt: "../../datetime.rkt")
         (only-in "../../misc.rkt" thread-first))

(define ((check-argument expected-argument-type) object-type argument-types)
  (if (val:type<=? (car argument-types) expected-argument-type)
      #t
      (format "Invalid argument type: Expected type ~a but got ~a." expected-argument-type (car argument-types))))

(define (check-set object-type argument-types)
  ;; object-type has format `(set ,type).
  (if (val:type<=? (car argument-types) (cadr object-type))
      #t
      (format "Invalid argument type for set of type ~a: Expected type ~a but got ~a."
              (cadr object-type)
              (cadr object-type)
              (car argument-types))))

(define op-string-suffix
  (create-operator #:name '>
                   #:object 'string
                   #:check-types (check-argument 'string)
                   #:body (λ (str sfx) (val:make-string (string-append (val:unwrap-string str) (val:unwrap-string sfx))))))
(define op-string-prefix
  (create-operator #:name '<
                   #:object 'string
                   #:check-types (check-argument 'string)
                   #:body (λ (str pfx) (val:make-string (string-append (val:unwrap-string pfx) (val:unwrap-string str))))))

(define op-number-add
  (create-operator #:name '+
                   #:object 'number
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (val:make-number (+ (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-subtract
  (create-operator #:name '-
                   #:object 'number
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (val:make-number (- (val:unwrap-number x) (val:unwrap-number y))))))

(define op-set-add
  (create-operator #:name '+
                   #:object 'set
                   #:check-types check-set
                   #:body (λ (xs x) (val:make-set (set-add (val:unwrap-set xs) x)))))
(define op-set-remove
  (create-operator #:name '-
                   #:object 'set
                   #:check-types check-set
                   #:body (λ (xs x) (val:make-set (set-remove (val:unwrap-set xs) x)))))

(define op-string-match?
  (create-operator #:name '/
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str sst) (val:make-boolean (string-contains? (string-downcase (val:unwrap-string str))
                                                                           (string-downcase (val:unwrap-string sst)))))))
(define op-string-prefix?
  (create-operator #:name '<
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str prf) (val:make-boolean (string-prefix? (val:unwrap-string str) (val:unwrap-string prf))))))
(define op-string-suffix?
  (create-operator #:name '>
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str sfx) (val:make-boolean (string-suffix? (val:unwrap-string str) (val:unwrap-string sfx))))))

(define op-set-contains?
  (create-operator #:name '+
                   #:object 'set
                   #:filter? #t
                   #:check-types check-set
                   #:body (λ (xs x) (val:make-boolean (set-member? (val:unwrap-set xs) x)))))
(define op-set-doesnt-contain?
  (create-operator #:name '-
                   #:object 'set
                   #:filter? #t
                   #:check-types check-set
                   #:body (λ (xs x) (val:make-boolean (not (set-member? (val:unwrap-set xs) x))))))

(define op-number-greater-than?
  (create-operator #:name '>
                   #:object 'number
                   #:filter? #t
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (val:make-boolean (> (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-less-than?
  (create-operator #:name '<
                   #:object 'number
                   #:filter? #t
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (val:make-boolean (< (val:unwrap-number x) (val:unwrap-number y))))))

(define op-date-after?
  (create-operator #:name '>
                   #:object 'date
                   #:filter? #t
                   #:check-types (check-argument 'date)
                   #:body (λ (x y) (val:make-boolean (dt:after? (val:unwrap-date x)
                                                                (val:unwrap-date y))))))
(define op-date-before?
  (create-operator #:name '<
                   #:object 'date
                   #:filter? #t
                   #:check-types (check-argument 'date)
                   #:body (λ (x y) (val:make-boolean (dt:before? (val:unwrap-date x)
                                                                 (val:unwrap-date y))))))
;;

(define builtin-operators
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
    (operator-definitions-add op-date-after?)
    (operator-definitions-add op-date-before?)
    ))

