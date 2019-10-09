#lang racket

(provide attribute
         get set update
         path

         table
         table.
         merge-tables

         define-species

         list. list.car list.cdr
         hash.)

;; Terminology:
;;
;; Attribute: A way of formalising the idea of an object "having" a particular
;; attribute. An attribute can be looked up on an object, and its value on an
;; object can be changed.
;;
;; Table: An unstructured mapping from keys to values. Differs from hash-map
;; only in that our concept of tables are implementation-agnostic (but currently
;; implemented using hash maps).
;;
;; Species: A class of tables, in the sense that a species is a description of a
;; particular kind of table that has certain keys. Species are implemented as an
;; implicit concept, not a first-class value.

(require (for-syntax racket/syntax))

;;; ATTRIBUTES

;; Construct an attribute with the given getter and setter.
(define (attribute #:get getter #:set setter)
  (cons getter setter))

;; Returns the value of an attribute on an object.
(define (get o attr)
  ((car attr) o))

;; Sets the value of an attribute on an object, returning a new object with its
;; attribute updated.
(define (set o attr x)
  ((cdr attr) o x))

;; Update the value of an attribute on an object by modifying the previous
;; value.
(define (update o attr f)
  (set o attr (f (get o attr))))

;; The path function is used to compose attributes. The argument is a list of
;; attributes, and the result is an attribute that looks up each of the given
;; attributes in turn.
;;
;; For example, (path (list.nth 2) (list.nth 0) (list.nth 1)) is an attribute
;; representing the second element of the first element of the third element of
;; a list.
(define (path . attrs)
  (if (empty? attrs)
      (attribute #:get (λ (o) o)
                 #:set (λ (o x) x))
      (attribute #:get (λ (o)
                         (get (get o (car attrs))
                              (apply path (cdr attrs))))
                 #:set (λ (o x)
                         (set o
                              (car attrs)
                              (set (get o (car attrs))
                                   (apply path (cdr attrs))
                                   x))))))

;;; TABLES

;; Create a table with the given mappings. Example: (table 'x 1 'y -3)
(define (table . pairs)
  (when (not (even? (length pairs)))
    (error "Arguments are not a sequence of key-value pairs."))
  (apply hash pairs))

;; Returns an attribute representing the value of the given key in a table.
;;
;; If the table does not already have the given key, an error is thrown. When
;; the goal is to add new keys to an existing table, use merge-tables instead of
;; setting this attribute.
(define (table. k)
  (attribute #:get (λ (t)
                     (when (not (hash-has-key? t k))
                       (error (format "No such key ~a in table ~s." k t)))
                     (hash-ref t k))
             #:set (λ (t x)
                     (when (not (hash-has-key? t k))
                       (error (format "No such key ~a in table ~s." k t)))
                     (hash-set t k x))))

;; Merges several tables together, returning a table with all the mappings of
;; both. If the tables contain the same key, then the values for that key must
;; be equal (by equal?).
(define (merge-tables . ts)
  (foldl (λ (t result)
           (foldl (λ (k result)
                    (when (and (hash-has-key? result k)
                               (not (equal? (hash-ref result k)
                                            (hash-ref t k))))
                      (error (format "Conflicting values for key ~a when merging tables: already has ~a->~s, which conflicts with ~a->~s."
                                     k k (hash-ref result k) k (hash-ref t k))))
                    (hash-set result k (hash-ref t k)))
                  result
                  (hash-keys t)))
         (hash)
         ts))

;;; SPECIES

;; Return predicate to check whether table t is of the species that contains
;; keys ks.
(define ((species-predicate ks) t)
  (andmap (λ (k) (hash-has-key? t k))
          ks))

;; Return attribute for key k in a given table of a given species.
;;
;; When is-given-species? is false, an error is thrown which refers to
;; given-species-name. Usually, is-given-species? will be a predicate returned
;; by species-predicate.
(define (species-attribute given-species-name is-given-species? k)
  (define (check! t)
    (when (not (is-given-species? t))
      (error (format "Table ~s is not a valid ~a." t given-species-name))))
  (attribute #:get (λ (t)
                     (check! t)
                     (get t (table. k)))
             #:set (λ (t x)
                     (check! t)
                     (set t (table. k) x))))

;; A species consists of a name together with a number of keys. The macro
;; define-species defines a constructor, a predicate and a number of attributes.
;;
;; Example:
;;   (define-species point (x y))               ; defines point?, point, point.x, point.y
;;   (point? (table 'x 2))                      ; => #f
;;   (update (point #:x 2 #:y -3) point.y abs)  ; => (point #:x 2 #:y 3)
(define-syntax (define-species stx)
  (define (syntax->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax->datum stx)))))
  (define (flat-map proc xs)
    (apply append (map proc xs)))
  (let ((name (cadr (syntax-e stx)))
        (keys (caddr (syntax-e stx))))
    (with-syntax (((attribute-definitions ...)
                   (map (λ (k)
                          #`(define #,(format-id k "~a.~a" name k)
                              (species-attribute '#,name (species-predicate '#,keys) '#,k)))
                        (syntax-e keys)))
                  (predicate-definition
                   #`(define #,(format-id name "~a?" name) (species-predicate '#,keys)))
                  (constructor-definition
                   (with-syntax (((params ...)
                                  (flat-map (λ (k) (list (syntax->keyword k) k))
                                            (syntax-e keys)))
                                 ((pairs ...)
                                  (flat-map (λ (k) (list #`(quote #,k) k))
                                            (syntax-e keys))))
                     #`(define (#,name params ...) (table pairs ...)))))
      #'(begin constructor-definition
               predicate-definition
               attribute-definitions ...))))

;;; COMMON ATTRIBUTES

;; Returns an attribute representing the nth element of a list.
(define (list. n)
  (attribute #:get (λ (l) (list-ref l n))
             #:set (λ (l x) (list-set l n x))))

;; Attributes representing the car and cdr of a list, repectively.
(define list.car (attribute #:get car #:set (λ (l x) (cons x (cdr l)))))
(define list.cdr (attribute #:get cdr #:set (λ (l x) (cons (car l) x))))

;; Returns an attribute representing the value of a given key in a hash map.
;;
;; The object may also be #f instead of a hash. In this case, getting any key
;; returns #f and setting a key returns a hash containing just the given
;; mapping. This behvior is useful for lookups inside nested hashes.
(define (hash. k #:default (default #f))
  (attribute #:get (λ (h) (and h (hash-ref h k default)))
             #:set (λ (h v) (if h (hash-set h k v) (hash k v)))))
