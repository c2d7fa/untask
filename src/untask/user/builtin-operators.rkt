#lang racket

(provide builtin-operators)

(require "../core/operator.rkt"
         (prefix-in val: "../core/value.rkt")
         (only-in "../../misc.rkt" thread-first))

(define op-string-suffix
  (create-operator #:name '>
                   #:object 'string
                   #:body (λ (str sfx) (val:make-string (string-append (val:unwrap-string str) (val:unwrap-string sfx))))))
(define op-string-prefix
  (create-operator #:name '<
                   #:object 'string
                   #:body (λ (str pfx) (val:make-string (string-append (val:unwrap-string pfx) (val:unwrap-string str))))))

(define op-number-add
  (create-operator #:name '+
                   #:object 'number
                   #:body (λ (x y) (val:make-number (+ (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-subtract
  (create-operator #:name '-
                   #:object 'number
                   #:body (λ (x y) (val:make-number (- (val:unwrap-number x) (val:unwrap-number y))))))

(define op-set-add
  (create-operator #:name '+
                   #:object 'set
                   #:body (λ (xs x) (val:make-set (set-add (val:unwrap-set xs) x)))))
(define op-set-remove
  (create-operator #:name '-
                   #:object 'set
                   #:body (λ (xs x) (val:make-set (set-remove (val:unwrap-set xs) x)))))

(define op-string-match?
  (create-operator #:name '/
                   #:object 'string
                   #:filter? #t
                   #:body (λ (str sst) (val:make-boolean (string-contains? (val:unwrap-string str) (val:unwrap-string sst))))))
(define op-string-prefix?
  (create-operator #:name '<
                   #:object 'string
                   #:filter? #t
                   #:body (λ (str prf) (val:make-boolean (string-prefix? (val:unwrap-string str) (val:unwrap-string prf))))))
(define op-string-suffix?
  (create-operator #:name '>
                   #:object 'string
                   #:filter? #t
                   #:body (λ (str sfx) (val:make-boolean (string-suffix? (val:unwrap-string str) (val:unwrap-string sfx))))))

(define op-set-contains?
  (create-operator #:name '+
                   #:object 'set
                   #:filter? #t
                   #:body (λ (xs x) (val:make-boolean (set-member? (val:unwrap-set xs) x)))))
(define op-set-doesnt-contain?
  (create-operator #:name '-
                   #:object 'set
                   #:filter? #t
                   #:body (λ (xs x) (val:make-boolean (not (set-member? (val:unwrap-set xs) x))))))

(define op-number-greater-than?
  (create-operator #:name '>
                   #:object 'number
                   #:filter? #t
                   #:body (λ (x y) (val:make-boolean (> (val:unwrap-number x) (val:unwrap-number y))))))
(define op-number-less-than?
  (create-operator #:name '<
                   #:object 'number
                   #:filter? #t
                   #:body (λ (x y) (val:make-boolean (< (val:unwrap-number x) (val:unwrap-number y))))))

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
    ))

