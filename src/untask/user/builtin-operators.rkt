#lang racket

(provide builtin-operators)

(require untask/src/untask/core/operator
         (prefix-in val: untask/src/untask/core/value)
         (prefix-in dt: untask/src/datetime)
         untask/src/squiggle)

(define ((check-argument expected-argument-type) object-type argument-value)
  (if (val:has-type? argument-value expected-argument-type)
      #t
      (format "Invalid argument type: Expected type ~a but got ~a." expected-argument-type (val:get-type argument-value))))

(define (check-set object-type argument-value)
  ;; object-type (and argument-type) has format `(set ,type).
  (if (val:has-type? argument-value (cadr object-type))
      #t
      (format "Invalid argument type for set of type ~a: Expected type ~a but got ~a."
              (cadr object-type)
              (cadr object-type)
              (val:get-type argument-value))))

(define (check-same object-type argument-value)
  ((check-argument object-type) object-type argument-value))

(define op-any-assign
  (create-operator #:name ':
                   #:object 'any
                   #:check-types check-same
                   #:body (λ (x y) y)))

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

;; TODO: We should probably add some new type representing a period of time, and
;; then add that to the date instead of always thinking in terms of days.
(define op-date-add-days
  (create-operator #:name '+
                   #:object 'date  ; TODO: May be optional! We should encode this somewhere.
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (and x (val:make-date (dt:add-days (val:unwrap-date x) (val:unwrap-number y)))))))
(define op-date-subtract-days
  (create-operator #:name '-
                   #:object 'date   ; TODO: See above
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (and x (val:make-date (dt:add-days (val:unwrap-date x) (- (val:unwrap-number y))))))))

(define op-any-equal?
  (create-operator #:name ':
                   #:object 'any
                   #:filter? #t
                   #:check-types check-same
                   #:body (λ (x y) (equal? x y))))

(define op-string-match?
  (create-operator #:name '/
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str sst) (string-contains? (string-downcase (val:unwrap-string str))
                                                         (string-downcase (val:unwrap-string sst))))))
(define op-string-prefix?
  (create-operator #:name '<
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str prf) (string-prefix? (val:unwrap-string str) (val:unwrap-string prf)))))
(define op-string-suffix?
  (create-operator #:name '>
                   #:object 'string
                   #:filter? #t
                   #:check-types (check-argument 'string)
                   #:body (λ (str sfx) (string-suffix? (val:unwrap-string str) (val:unwrap-string sfx)))))

(define op-set-contains?
  (create-operator #:name '+
                   #:object 'set
                   #:filter? #t
                   #:check-types check-set
                   #:body (λ (xs x) (set-member? (val:unwrap-set xs) x))))
(define op-set-doesnt-contain?
  (create-operator #:name '-
                   #:object 'set
                   #:filter? #t
                   #:check-types check-set
                   #:body (λ (xs x) (not (set-member? (val:unwrap-set xs) x)))))

(define op-number-greater-than?
  (create-operator #:name '>
                   #:object 'number
                   #:filter? #t
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (> (val:unwrap-number x) (val:unwrap-number y)))))
(define op-number-less-than?
  (create-operator #:name '<
                   #:object 'number
                   #:filter? #t
                   #:check-types (check-argument 'number)
                   #:body (λ (x y) (< (val:unwrap-number x) (val:unwrap-number y)))))

(define op-date-after?
  (create-operator #:name '>
                   #:object 'date   ; TODO: May be optional! We should encode this somewhere.
                   #:filter? #t
                   #:check-types (check-argument 'date)
                   #:body (λ (x y) (and x (dt:after? (val:unwrap-date x) (val:unwrap-date y))))))
(define op-date-before?
  (create-operator #:name '<
                   #:object 'date   ; TODO: May be optional! We should encode this somewhere.
                   #:filter? #t
                   #:check-types (check-argument 'date)
                   #:body (λ (x y) (and x (dt:before? (val:unwrap-date x) (val:unwrap-date y))))))
;;

(define builtin-operators
  (~> operator-definitions-empty
    (operator-definitions-add op-any-assign)
    (operator-definitions-add op-string-suffix)
    (operator-definitions-add op-string-prefix)
    (operator-definitions-add op-number-add)
    (operator-definitions-add op-number-subtract)
    (operator-definitions-add op-set-add)
    (operator-definitions-add op-set-remove)
    (operator-definitions-add op-date-add-days)
    (operator-definitions-add op-date-subtract-days)

    (operator-definitions-add op-any-equal?)
    (operator-definitions-add op-string-match?)
    (operator-definitions-add op-string-prefix?)
    (operator-definitions-add op-string-suffix?)
    (operator-definitions-add op-set-contains?)
    (operator-definitions-add op-set-doesnt-contain?)
    (operator-definitions-add op-number-greater-than?)
    (operator-definitions-add op-number-less-than?)
    (operator-definitions-add op-date-after?)
    (operator-definitions-add op-date-before?)))
