#lang racket

(provide save-state-to-string
         load-state-from-string
         save-state
         load-state)

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

(define (serialize-active-contexts active-contexts) (set->list active-contexts))
(define (deserialize-active-contexts active-contexts) (list->set active-contexts))

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

(define (serialize-defined-contexts defined-contexts)
  (define (serialize-context x)
    (list (serialize-fm-expression (a:get (x context:context.filter)))
          (serialize-fm-expression (a:get (x context:context.modify)))))
  (serialize-hash defined-contexts #:serialize-value serialize-context))
(define (deserialize-defined-contexts defined-contexts)
  (define (deserialize-context x)
    (context:context #:filter (deserialize-fm-expression (car x))
                     #:modify (deserialize-fm-expression (cadr x))))
  (deserialize-hash defined-contexts #:deserialize-value deserialize-context))

(define (serialize-item-data-properties properties)
  (serialize-hash properties #:serialize-value (λ (h) (serialize-hash h #:serialize-value serialize-value))))
(define (deserialize-item-data-properties properties)
  (deserialize-hash properties #:deserialize-value (λ (h) (deserialize-hash h #:deserialize-value deserialize-value))))

(define (serialize-state st)
  (serialize-hash (hash 'file-version (file-version)
                        'all-contexts (serialize-defined-contexts (a:get (st state:state.defined-contexts context:contexts.definitions)))
                        'active-contexts (serialize-active-contexts (a:get (st state:state.active-contexts)))
                        'next-item-id (a:get (st state:state.item-data item:item-data.next))
                        'item-property-data (serialize-item-data-properties (a:get (st state:state.item-data item:item-data.properties))))))
(define (deserialize-state st #:open-file open-file)
  (define h (deserialize-hash st))
  (when (not (= (hash-ref h 'file-version) (file-version)))
    (error (format "This version of Untask can only read file format version ~A, but the given file has version ~A." (file-version) (hash-ref h 'file-version))))
  (state:state #:defined-contexts (context:contexts #:definitions (deserialize-defined-contexts (hash-ref h 'all-contexts)))
               #:active-contexts (deserialize-active-contexts (hash-ref h 'active-contexts))
               #:open-file open-file
               #:item-data (item:item-data #:next (hash-ref h 'next-item-id)
                                           #:properties (deserialize-item-data-properties (hash-ref h 'item-property-data)))))

(define (save-state-to-string st)
  (~s (serialize-state st)))

(define (load-state-from-string st #:open-file open-file)
  (deserialize-state (read (open-input-string st)) open-file))

(define (save-state st)
  (with-output-to-file (a:get (st state:state.open-file)) #:exists 'replace
    (thunk (displayln (save-state-to-string st)))))

(define (load-state path)
  (deserialize-state (read (open-input-file path))
                     #:open-file path))
