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
         (only-in untask/src/misc flat-map)
         untask/src/squiggle
         racket/pretty)

(define file-version (make-parameter 0))

(define serializable-properties '(status description notes tags urgency depends blocks children parents date wait color effort order))

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
         (c:available context-state)))))
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
  (~> (a:get-path (st state.context-state)) (c:activated)))
;; Returns a context-state with the new contexts added.
(define (deserialize-activated-contexts cst x)
  (foldl (λ (name cst)
           (c:activate cst name))
         cst
         x))

(define (serialize-item-property-data item-state)
  (flat-map (λ (item)
              (let ((properties*
                     (flat-map (λ (property-name)
                                 (let ((value (i:get item-state item property-name)))  ; Don't save empty property
                                   (if value
                                       (list (list property-name (serialize-value value)))
                                       (list))))
                               serializable-properties)))
                (if (empty? properties*)
                    (list)
                    (list (list item properties*)))))
            (i:items item-state)))

(define (deserialize-item-state state*/hash)
  (foldl (λ (x item-state)
           (match-let ((`(,item ,property-name ,value*) x))
             (i:set item-state item property-name (deserialize-value value*))))
         (i:prepare-deserialization-empty-state (hash-ref state*/hash 'next-item-id))
         (flat-map (λ (item-and-properties)
                     (map (λ (name-and-value)
                            (list (first item-and-properties)
                                  (first name-and-value)
                                  (second name-and-value)))
                          (second item-and-properties)))
                   (hash-ref state*/hash 'item-property-data))))

(define (serialize-state state)
  (serialize-hash (hash 'version (file-version)
                        'all-contexts (serialize-defined-contexts state)
                        'active-contexts (serialize-activated-contexts state)
                        'next-item-id (i:prepare-serialization-next-id (a:get-path (state state.item-state)))
                        'item-property-data (serialize-item-property-data (a:get-path (state state.item-state))))))

(define (deserialize-state state* #:open-file open-file)
  (define state*/hash (deserialize-hash state*))
  (define version (hash-ref state*/hash 'version))
  (when (not (= version 0))
    (error (format "This version of Untask can only read file format version 0, but the given file has version ~A." version)))
  (parameterize ((file-version version))
    (state #:context-state (~> c:empty-state
                               (deserialize-defined-contexts (hash-ref state*/hash 'all-contexts))
                               (deserialize-activated-contexts (hash-ref state*/hash 'active-contexts)))
           #:item-state (deserialize-item-state state*/hash)
           #:open-file open-file)))

;; (save-state-to-string state)
;;
;; Serializes the given state to a string that can later be restored with
;; load-state-from-string.
;;
;; (load-state-from-string string #:open-file open-file)
;;
;; Deserializes the state strored in the given string. The currently open file
;; is not saved when serializing, so it must be passed explicitly.
(define (save-state-to-string st)
  (pretty-format #:mode 'write (serialize-state st)))

(define (load-state-from-string st #:open-file open-file)
  (deserialize-state (read (open-input-string st))
                     #:open-file open-file))

;; (save-state state)
;; (load-state path)
;;
;; Save/load state to/from a file on disk. When saving, the currently open file
;; is used as the path to save.
(define (save-state st)
  (with-output-to-file (a:get-path (st state.open-file)) #:exists 'replace
    (thunk (displayln (save-state-to-string st)))))

(define (load-state path)
  (deserialize-state (read (open-input-file path))
                     #:open-file path))
