#lang racket

(provide evaluate-modify-expression
         modify-items)

(require
 (prefix-in operators: untask/src/untask/core/operator)
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in val: untask/src/untask/core/value)
 (prefix-in bp: untask/src/untask/properties/builtin)
 untask/src/untask/user/builtin-operators
 untask/src/squiggle
 racket/system)

;; TODO: Correctly detect editor on other platforms.
(define (result-of-editor-on! str)
  (let ((temp-file (make-temporary-file "/tmp/untask-~a")))
    (with-output-to-file temp-file #:exists 'truncate
      (thunk (display str)))
    (system (format "$EDITOR ~a" temp-file))
    (call-with-input-file temp-file (lambda (in) (port->string in)))))

;; Modify the given item accoring to modify-expression.
(define (evaluate-modify-expression modify-expression item-state item)
  (match modify-expression
    (`(edit)
     (let* ((old-description (val:unwrap-string (p:get item-state item (bp:ref 'description))))
            (old-notes (val:unwrap-string (p:get item-state item (bp:ref 'notes))))
            (str (string-trim (string-join (list old-description old-notes) "\n\n")))
            (text (result-of-editor-on! str))
            (lines (string-split (string-trim text) "\n"))
            (new-description (car lines))
            (new-notes (string-trim (string-join (cdr lines) "\n"))))
       (~> item-state
           (p:set item (bp:ref 'description) (val:make-string new-description))
           (p:set item (bp:ref 'notes) (val:make-string new-notes)))))
    (`(edit ,property)
     (p:set item-state item (bp:ref property) (val:make-string (result-of-editor-on! (val:unwrap-string (p:get item-state item (bp:ref property)))))))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     (p:set item-state item (bp:ref property) (operators:evaluate-operator-expression
                                               builtin-operators
                                               #:object (p:get item-state item (bp:ref property))
                                               #:operator operator
                                               #:argument (val:evaluate-literal literal-expr)
                                               #:object-type (p:type (bp:ref property))
                                               #:filter? #f)))
    (`(and ,subexpressions ...)
     (foldl (λ (subexpression item-state)
              (evaluate-modify-expression subexpression item-state item))
            item-state
            subexpressions))
    ((list) item-state)))

;; Returns new item-state after modifying all items in the given set accoring to
;; modify-expression.
(define (modify-items item-state items modify-expression)
  (foldl (λ (item item-state)
           (evaluate-modify-expression modify-expression item-state item))
         item-state
         (set->list items)))
