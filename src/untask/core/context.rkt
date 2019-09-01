#lang racket

(provide (all-defined-out))

(require (prefix-in a: "../../attribute.rkt"))

(a:define-record context (filter modify))
(a:define-record contexts (definitions))

(define empty-contexts
  (contexts #:definitions (hash)))

(define (available-contexts contexts)
  (hash-keys (a:get (contexts contexts.definitions))))

(define (add-context contexts name #:filter filter-expression #:modify modify-expression)
  (a:set (contexts contexts.definitions (a:hash.key name))
         (context #:filter filter-expression #:modify modify-expression)))

(define (remove-context contexts name)
  (a:update (contexts contexts.definitions)
            (λ (x) (hash-remove x name))))

(define (contexts.named name)
  (define (get-context definitions)
    (a:get (definitions
             (a:hash.key name #:default (context #:filter '() #:modify '())))))
  (a:make-attribute #:get (λ (x) (get-context (a:get (x contexts.definitions))))
                    #:set (λ (x v) (a:set (x contexts.definitions (a:hash.key name)) v))))

(define (apply-context-filter context filter-expression)
  (list 'and
        (a:get (context context.filter))
        filter-expression))

(define (apply-context-modify context modify-expression)
  (list 'and
        (a:get (context context.modify))
        modify-expression))
