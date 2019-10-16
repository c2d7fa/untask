#lang racket

(provide state?
         empty-state
         items

         new
         new/state
         new/item
         remove
         copy

         get
         set
         update

         load-state
         dump-state)

;; In this module, the term item is used to describe the numeric IDs that refer
;; to a particular item rather than the data associated with that item. Each
;; such item is associated with a set of properties, where each property is a
;; mapping from a name to a value (in the sense of Untask values, defined in the
;; "value" module).
;;
;; Note that the "property" module extends the meaning of the term property, by
;; assigning types and dynamic behviour to the properties. Usually, item
;; properties are not changed by using this module, but rather by using the
;; property module.
;;
;; The program needs to keep track of the available items, their properties and
;; their IDs. These things are handled by this module.

(require (prefix-in a: "../../attribute.rkt")
         "../../squiggle.rkt")

;; The state relating to items stores the ID of the next item to be created. The
;; properties are stored as a hash-map from items to a hash map from property
;; names to property values.
(a:define-species state (next properties))
(define empty-state (state #:next 0 #:properties (hash)))

;; Create a new item which has no properties set. Returns two values: The
;; updated state and the item.
(define (new state)
  (let ((next (a:get-path (state state.next))))
    (values (a:update-path (state state.next) add1)
            next)))

;; new/state - Create a new item, returning only the updated item state.
;; new/item  - Return the item that would be created by new.
;;
;; Note that
;;   (let ((state* (new/state state))
;;         (item*  (new/item state)))
;;     ...)
;; is equivalent to
;;   (let-values (((state* item*) (new state)))
;;     ...)
(define (new/state state)
  (let-values (((state* item*) (new state)))
    state*))
(define (new/item state)
  (let-values (((state* item*) (new state)))
    item*))

;; Get the value of the given property. Returns the updated item state.
;;
;; If the given item does not exist or the given property is not set, return #f.
(define (get state item property-name)
  (a:get-path (state state.properties (a:hash. item) (a:hash. property-name))))

;; Set the value of the given property. Returns the updated item state.
;;
;; If the given item does not exist, throws an error.
(define (set state item property-name property-value)
  (when (<= (a:get-path (state state.next)) item)
    (error (format "Cannot set property '~A' of item ~A, because no such item exists; the next item to be added is ~A."
                   property-name item (a:get-path (state state.next)))))
  (a:set-path (state state.properties (a:hash. item) (a:hash. property-name)) property-value))

;; Update the value of the given property. Returns the updated item state.
;;
;; If the given item does not exist, throws an error. If the given property is
;; not set, calls the updating function with #f as the argument.
(define (update state item property-name f)
  (set state item property-name (f (get state item property-name))))

;; Remove an item. Returns the updated item state.
(define (remove state item)
  (a:update-path (state state.properties)
                 (λ> (hash-remove item))))

;; Returns the names of all set (i.e. non-#f) properties of the given item.
(define (property-names state item)
  (filter (λ (property-name)
            (get state item property-name))
          (hash-keys (a:get-path (state state.properties (a:hash. item))))))

;; Copy all set properties of source item into destination item. Returns updated
;; item data.
;;
;; If source item does not exist, does nothing. If destination item does not
;; exist, throws an error.
(define (copy state source destination)
  (foldl (λ (property-name state)
           (set state destination property-name (get state source property-name)))
         state
         (property-names state source)))

;; Returns all items that have set properties.
(define (items state)
  (filter (λ (item)
            (not (empty? (property-names state item))))
          (hash-keys (a:get-path (state state.properties)))))

;; Initialize a state from the given parameters:
;; 1. next-item is the ID of the next item to be created;
;; 2. property-map is a hash map from items to hash maps from property names to
;;    property values.
(define (load-state next-item property-map)
  (state #:next next-item #:properties property-map))

;; Return the values required by load-state to recreate the given state.
(define (dump-state state)
  (values (a:get-path (state state.next))
          (a:get-path (state state.properties))))
