#lang racket

(provide save-state-to-string
         load-state-from-string
         save-state
         load-state)

(require untask/src/untask/core/state
         (prefix-in val: untask/src/untask/core/value)
         (prefix-in c: untask/src/untask/core/context)
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in dt: untask/src/datetime)
         (prefix-in a: untask/src/attribute)
         untask/src/squiggle
         racket/pretty)

(define file-version (make-parameter 0))

(define (serialize-value x)
  (cond
    ((val:string-value? x) `(string ,(val:unwrap-string x)))
    ((val:number-value? x) `(number ,(val:unwrap-number x)))
    ((val:item-value? x) `(item ,(val:unwrap-item x)))
    ((val:boolean-value? x) `(boolean ,(val:unwrap-boolean x)))
    ((val:date-value? x) (let ((d (val:unwrap-date x)))
                           `(date (,(dt:datetime-year d)
                                   ,(dt:datetime-month d)
                                   ,(dt:datetime-day d)
                                   ,(dt:datetime-hour d)
                                   ,(dt:datetime-minute d)))))
    ((val:set-value? x) `(set ,(map serialize-value (set->list (val:unwrap-set x)))))
    ((val:empty-value? x) #f)
    (else (error (format "Unable to serialize value: ~S" x)))))

(define (deserialize-value x)
  (match x
    (`(string ,s) (val:make-string s))
    (`(number ,n) (val:make-number n))
    (`(item ,i) (val:make-item i))
    (`(boolean ,b) (val:make-boolean b))
    (`(date (,year ,month ,day ,hour ,minute)) (val:make-date (dt:datetime year month day hour minute)))
    (`(set ,s) (val:make-set (apply set (map deserialize-value s))))
    (#f #f)
    (else (error (format "Unable to deserialize value: ~S" x)))))

(define (serialize-hash h #:serialize-key (serialize-key identity) #:serialize-value (serialize-value identity))
  (map (λ (k) (list (serialize-key k) (serialize-value (hash-ref h k))))
       (hash-keys h)))

(define (deserialize-hash kvs #:deserialize-key (deserialize-key identity) #:deserialize-value (deserialize-value identity))
  (apply hash (apply append (map (λ (kv)
                                   (list (deserialize-key (car kv))
                                         (deserialize-value (cadr kv))))
                                 kvs))))

;; NOTE: This depends on the representations of literals.
(define (serialize-literal x)
  (match x
    (`(string . ,s) `(string ,s))
    (`(number . ,n) `(number ,n))
    (`(item . ,i) `(item ,i))
    (`(boolean . ,b) `(boolean ,b))
    (`(set . ,s) `(set . ,(map serialize-literal s)))
    (`(date . ,d) `(date . ,d))
    (else (error (format "Could not serialize literal: ~S" x)))))
(define (deserialize-literal x)
  (match x
    (`(string ,s) `(string . ,s))
    (`(number ,n) `(number . ,n))
    (`(item ,i) `(item . ,i))
    (`(boolean ,b) `(boolean . ,b))
    (`(set . ,s) `(set . ,(map deserialize-literal s)))
    (`(date . ,d) `(date . ,d))
    (else (error (format "Could not deserialize literal: ~S" x)))))

;; NOTE: This depends on the representation of filer expressions and modify
;; expressions.
(define (serialize-fm-expression fme)
  (match fme
    (`(and ,subexprs ...) `(and ,@(map serialize-fm-expression subexprs)))
    (`(or ,subexprs ...) `(or ,@(map serialize-fm-expression subexprs)))
    (`(not ,subexpr) `(not ,(serialize-fm-expression subexpr)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     `(,property ,operator ,(serialize-literal literal-expr)))
    (`(item . ,id) `(item ,id))
    ('() '())
    (else (error (format "Could not serialize filter/modify expression: ~S" fme)))))
(define (deserialize-fm-expression fme)
  (match fme
    (`(and ,subexprs ...) `(and ,@(map deserialize-fm-expression subexprs)))
    (`(or ,subexprs ...) `(or ,@(map deserialize-fm-expression subexprs)))
    (`(not ,subexpr) `(not ,(deserialize-fm-expression subexpr)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     `(,property ,operator ,(deserialize-literal literal-expr)))
    (`(item ,id) `(item . ,id))
    ('() '())
    (else (error (format "Could not deserialize filter/modify expression: ~S" fme)))))

(define (serialize-defined-contexts st)
  (define context-state (a:get-path (st state.context-state)))
  (serialize-hash
   (make-immutable-hash
    (map (λ (name)
           (cons name
                 (list (serialize-fm-expression (c:filter context-state name))
                       (serialize-fm-expression (c:modify context-state name)))))
         (c:available-names context-state)))))
;; Returns a context-state with the new contexts added.
(define (deserialize-defined-contexts ctx-st defined-contexts)
  (define h (deserialize-hash defined-contexts))
  (foldl (λ (name st)
           (c:register st name
                         #:filter (deserialize-fm-expression (car (hash-ref h name)))
                         #:modify (deserialize-fm-expression (cadr (hash-ref h name)))))
         ctx-st
         (hash-keys h)))

(define (serialize-activated-contexts st)
  (~> (a:get-path (st state.context-state)) (c:activated-names)))
;; Returns a context-state with the new contexts added.
(define (deserialize-activated-contexts cst x)
  (foldl (λ (name cst)
           (c:activate cst name))
         cst
         x))

(define (serialize-item-property-data properties)
  (serialize-hash properties #:serialize-value (λ (h) (serialize-hash h #:serialize-value serialize-value))))
(define (deserialize-item-property-data properties)
  (deserialize-hash properties #:deserialize-value (λ (h) (deserialize-hash h #:deserialize-value deserialize-value))))

(define (serialize-state st)
  (let-values (((next-item property-map) (i:dump-state (a:get-path (st state.item-state)))))
    (serialize-hash (hash 'version (file-version)
                          'all-contexts (serialize-defined-contexts st)
                          'active-contexts (serialize-activated-contexts st)
                          'next-item-id next-item
                          'item-property-data (serialize-item-property-data property-map)))))
(define (deserialize-state st #:open-file open-file)
  (define h (deserialize-hash st))
  (when (not (= (hash-ref h 'version) (file-version)))
    (error (format "This version of Untask can only read file format version ~A, but the given file has version ~A." (file-version) (hash-ref h 'version))))
  (state #:context-state (~> c:empty-state
                             (deserialize-defined-contexts (hash-ref h 'all-contexts))
                             (deserialize-activated-contexts (hash-ref h 'active-contexts)))
         #:item-state (i:load-state (hash-ref h 'next-item-id)
                                    (deserialize-item-property-data (hash-ref h 'item-property-data)))
         #:open-file open-file))

(define (save-state-to-string st)
  (pretty-format #:mode 'write (serialize-state st)))

(define (load-state-from-string st #:open-file open-file)
  (deserialize-state (read (open-input-string st))
                     #:open-file open-file))

(define (save-state st)
  (with-output-to-file (a:get-path (st state.open-file)) #:exists 'replace
    (thunk (displayln (save-state-to-string st)))))

(define (load-state path)
  (deserialize-state (read (open-input-file path))
                     #:open-file path))