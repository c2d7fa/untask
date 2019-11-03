#lang racket

(provide order-property)

(require
 (prefix-in p: untask/src/untask/core/property)
 (prefix-in i: untask/src/untask/core/item)
 (prefix-in v: untask/src/untask/core/value)
 (only-in untask/src/untask/properties/date date-property))

(define (translate-order item-state item value)
  ;; TODO: This is not great code style
  (define date (p:get item-state item date-property))
  (if (not date)
      item-state
      (let* ((neighbours (filter (λ (item*)
                                   (and (not (equal? item* item))
                                        (equal? (p:get item-state item* date-property) date)
                                        (i:get item-state item* 'order)))
                                 (i:items item-state)))
             (last-order (length neighbours))
             (old-value (i:get item-state item 'order)))
        (if (not value)
            ;; Removing order; nudge neighbours together
            (let* ((nudged-neighbours (if old-value
                                          (filter (λ (neighbour)
                                                    (<= (v:unwrap-number old-value) (v:unwrap-number (i:get item-state neighbour 'order))))
                                                  neighbours)
                                          '())))
              (foldl (λ (nudged-neighbour item-state)
                       (i:update item-state nudged-neighbour 'order (λ (value) (v:make-number (sub1 (v:unwrap-number value))))))
                     (i:set item-state item 'order #f)
                     nudged-neighbours))
            (if (> (v:unwrap-number value) last-order)
                ;; Trying to set order past last; set highest possible instead
                (i:set item-state item 'order (v:make-number (add1 last-order)))
                (let* ((nudged-neighbours (if old-value
                                              (filter (λ (neighbour)
                                                        (and (<= (v:unwrap-number value) (v:unwrap-number (i:get item-state neighbour 'order)))
                                                             (< (v:unwrap-number (i:get item-state neighbour 'order)) (v:unwrap-number old-value))))
                                                      neighbours)
                                              (filter (λ (neighbour)
                                                        (<= (v:unwrap-number value) (v:unwrap-number (i:get item-state neighbour 'order))))
                                                      neighbours))))
                  (foldl (λ (nudged-neighbour item-state)
                           (i:update item-state nudged-neighbour 'order (λ (value) (v:make-number (add1 (v:unwrap-number value))))))
                         (i:set item-state item 'order value)
                         nudged-neighbours)))))))

(define order-property
  (p:property #:name 'order
              #:type '(opt number)
              #:translate translate-order))
