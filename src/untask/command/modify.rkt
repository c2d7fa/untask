#lang racket

(provide (all-defined-out))

(require
 (prefix-in operators: "../core/operator.rkt")
 (prefix-in item: "../core/item.rkt")
 (prefix-in val: "../core/value.rkt")

 "../user/builtin-operators.rkt"
 "../properties/builtin.rkt"

 racket/system)

;; TODO: Correctly detect editor on other platforms.
(define (result-of-editor-on! str)
  (let ((temp-file (make-temporary-file "/tmp/untask-~a")))
    (with-output-to-file temp-file #:exists 'truncate
      (thunk (display str)))
    (system (format "$EDITOR ~a" temp-file))
    (call-with-input-file temp-file (lambda (in) (port->string in)))))

;; Take a modify expression and return a function that will update the
;; given item in the given item-data according to the
;; modify-expression.
(define (evaluate-modify-expression modify-expression item-data item)
  (match modify-expression
    (`(edit ,property)
     (set-property-by-key item-data item property (val:make-string (result-of-editor-on! (val:unwrap-string (get-property-by-key item-data item property))))))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (set-property-by-key item-data item property (operators:evaluate-operator-expression
                                                   builtin-operators
                                                   #:object (get-property-by-key item-data item property)
                                                   #:operator operator
                                                   #:argument (val:evaluate-literal literal-expr)
                                                   #:object-type (get-property-type-type property)
                                                   #:filter? #f)))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-data)
              (evaluate-modify-expression subexpression item-data item))
            item-data
            subexpressions))
    ((list)
     item-data)))

;; Returns new-item-data after modifying all items in item-data according to
;; modify-expression.
(define (modify-items item-data items modify-expression)
  (foldl (λ (item item-data)
           (evaluate-modify-expression modify-expression item-data item))
         item-data
         (set->list items)))
