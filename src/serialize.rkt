#lang racket

(provide serialize-state
         deserialize-state
         save-state-to-file
         save-state
         load-state)

;; TODO: This module currently depends on implementation details of basically
;; all modules. This is probably a bad idea. I should think about how to improve
;; this situation.

(require (prefix-in val: "untask/core/value.rkt")
         (prefix-in dt: "datetime.rkt"))

;; In order to be able to easily write and read to files in a consistent way, we
;; want the values that we read/write to be simple S-expressions which may
;; contains symbols, numbers and strings.
;;
;; Creating such an S-expression from a more complex representation we call
;; "preparing", and we call the opposite process "unpreparing".

(define (prepare-value v)
  (match v
    ;; TODO: Dependency on value module
    (`(string . ,s) `(string ,s))
    (`(number . ,n) `(number ,n))
    (`(item . ,i) `(item ,i))
    (`(boolean . ,b) `(boolean ,b))
    (`(date . ,d) `(date ,d))       ; TODO: Dependency on datetime module; representation could change
    (`(set . ,s) `(set ,@(map prepare-value (set->list s))))
    (#f #f)
    (else (error (format "Unknown value type for ~S" v)))))

(define (unprepare-value p)
  (match p
    (`(string ,s) (val:make-string s))
    (`(number ,n) (val:make-number n))
    (`(item ,i) (val:make-item i))
    (`(boolean ,b) (val:make-boolean b))
    (`(date (,year ,month ,day ,hour ,minute)) (val:make-date (dt:datetime year month day hour minute)))
    (`(set ,@s) (val:make-set (apply set (map unprepare-value s))))
    (#f #f)
    (else (error (format "Unknown value type for ~S" p)))))

(define (prepare-hash h #:prepare-k prepare-k #:prepare-v prepare-v)
  (map (λ (k) (list (prepare-k k) (prepare-v (hash-ref h k))))
       (hash-keys h)))

(define (unprepare-hash kvs #:unprepare-k unprepare-k #:unprepare-v unprepare-v)
  (apply hash (apply append (map (λ (kv)
                                   (list (unprepare-k (car kv))
                                         (unprepare-v (cadr kv))))
                                 kvs))))

(require (prefix-in export: "./untask/core/export.rkt"))

(define st (export:read-state-from-file "../tasks.t"))

(require (prefix-in a: "attribute.rkt")
         (prefix-in state: "./untask/core/state.rkt")
         (prefix-in context: "./untask/core/context.rkt")
         (prefix-in item: "./untask/core/item.rkt"))

;; TODO: Dependencies on state (probably OK) and context (probably not OK)
(define (prepare-active-contexts active-contexts)
  (set->list active-contexts))
(define (unprepare-active-contexts active-contexts)
  (list->set active-contexts))

;; TODO: This depends on the representations of literals.
(define (prepare-literal l)
  (match l
    (`(string . ,s) `(string ,s))
    (`(number . ,n) `(number ,n))
    (`(item . ,i) `(item ,i))
    (`(boolean . ,b) `(boolean ,b))
    (`(set . ,s) `(set . ,(map prepare-literal s)))
    (`(date . ,d) `(date . ,d))))
(define (unprepare-literal l)
  (match l
    (`(string ,s) `(string . ,s))
    (`(number ,n) `(number . ,n))
    (`(item ,i) `(item . ,i))
    (`(boolean ,b) `(boolean . ,b))
    (`(set . ,s) `(set . ,(map unprepare-literal s)))
    (`(date . ,d) `(date . ,d))))

;; TODO: This depends on the representation of filter expressions and modify
;; expressions. Consider if that is OK.
(define (prepare-fm-expression fme)
  (match fme
    (`(and ,subexprs ...) `(and ,@(map prepare-fm-expression subexprs)))
    (`(or ,subexprs ...) `(or ,@(map prepare-fm-expression subexprs)))
    (`(not ,subexpr) `(not ,(prepare-fm-expression subexpr)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     `(,property ,operator ,(prepare-literal literal-expr)))
    (`(item . ,id) `(item ,id))
    ('() '())))
(define (unprepare-fm-expression fme)
  (match fme
    (`(and ,subexprs ...) `(and ,@(map unprepare-fm-expression subexprs)))
    (`(or ,subexprs ...) `(or ,@(map unprepare-fm-expression subexprs)))
    (`(not ,subexpr) `(not ,(unprepare-fm-expression subexpr)))
    (`(,property ,operator ,literal-expr) #:when (symbol? operator)
     `(,property ,operator ,(unprepare-literal literal-expr)))
    (`(item ,id) `(item . ,id))
    ('() '())))


;; TODO: Dependencies on context
(define (prepare-defined-contexts defined-contexts)
  (prepare-hash defined-contexts
                #:prepare-k identity
                #:prepare-v (λ (context)
                              (list (prepare-fm-expression (a:get (context context:context.filter)))
                                    (prepare-fm-expression (a:get (context context:context.modify)))))))
(define (unprepare-defined-contexts defined-contexts)
  (unprepare-hash defined-contexts
                  #:unprepare-k identity
                  #:unprepare-v (λ (context)
                                  (context:context #:filter (unprepare-fm-expression (car context))
                                                   #:modify (unprepare-fm-expression (cadr context))))))

(define (prepare-item-data-properties properties)
  (prepare-hash properties
                #:prepare-k identity
                #:prepare-v (λ (props)
                              (prepare-hash props
                                            #:prepare-k identity
                                            #:prepare-v prepare-value))))
(define (unprepare-item-data-properties properties)
  (unprepare-hash properties
                  #:unprepare-k identity
                  #:unprepare-v (λ (props)
                                  (unprepare-hash props
                                                  #:unprepare-k identity
                                                  #:unprepare-v unprepare-value))))

(define (prepare-state st)
  (list (prepare-defined-contexts (a:get (st state:state.defined-contexts context:contexts.definitions)))
        (prepare-active-contexts (a:get (st state:state.active-contexts)))
        (a:get (st state:state.item-data item:item-data.next))
        (prepare-item-data-properties (a:get (st state:state.item-data item:item-data.properties)))))
(define (unprepare-state st open-file)
  (match st
    ((list defined-contexts active-contexts next-id item-data-properties)
     (state:state #:defined-contexts (context:contexts #:definitions (unprepare-defined-contexts defined-contexts))
                  #:active-contexts (unprepare-active-contexts active-contexts)
                  #:open-file open-file
                  #:item-data (item:item-data #:next next-id
                                              #:properties (unprepare-item-data-properties item-data-properties))))))

(define (serialize-state st)
  (~s (prepare-state st)))

(define (deserialize-state st #:open-file open-file)
  (unprepare-state (read (open-input-string st)) open-file))

(define (save-state-to-file st path)
  (with-output-to-file "test.t" #:exists 'replace
    (thunk (displayln (serialize-state st)))))

(define (save-state st)
  (save-state-to-file st (a:get (st state:state.open-file))))

(define (load-state path)
  (deserialize-state (port->string (open-input-file path) #:close? #t)
                     #:open-file path))
