#lang racket

(require rackunit untask/test/util
         (prefix-in cmd: untask/src/untask/command/command)

         untask/src/squiggle
         (prefix-in s: untask/src/untask/core/state)
         (prefix-in c: untask/src/untask/core/context)
         (prefix-in v: untask/src/untask/core/value)
         (prefix-in i: untask/src/untask/core/item)
         (prefix-in dt: untask/src/datetime)
         (prefix-in a: untask/src/attribute))

(provide command-tests)

;; Example state with dependencies and links between four items.
;;
;; 4 has child 1
;; 4 has child 2
;; 1 depends on 2
;; 2 depends on 3
;; 5 has child 1
;;
;; 1 has urgency 1; all others have urgency 0
(define st1 (load-example "example-2.t"))

;; Example state with dates.
;;
;; 1 has no date
;; 2, 3, 5 have date 2019-10-23
;; 4 has date 2019-10-27
;;
;; 2 has urgency 2
;; 5 has urgency 1
;; 3 has urgency 0
(define st2 (load-example "example-3.t"))

;; Example with urgency and tags. Has 5 items.
(define st3 (load-example "example-4.t"))

(define command-tests
  ;; TODO: Test exceptions
  (test-suite "Commands"
    (test-suite "List"
      (test-case "Example matches correct items"
        (check set=? (cmd:list st3 #:filter '(and (tags + (string . "tag"))
                                                  (urgency < (number . 3))))
                     '(3 4)))

      (test-case "Results are sorted by urgency"
        (check-equal? (cmd:list st3 #:filter '(not (tags : (set))))
                      '(2 3 5 4)))

      (test-case "Matching takes contexts into account"
        (check set=? (cmd:list st3 #:filter '()) '(1 2 3 4 5))
        (let ((st* (a:update-path (st3 s:state.context-state)
                                  (λ> (c:activate "tagged")))))
          (check set=? (cmd:list st* #:filter '()) '(2 3 4)))))

    (test-suite "Tree"
      (test-case "Tree of non-matching filter expression gives no trees"
        (check-equal? (cmd:tree st1 #:filter '(not ()) #:post-filter '()) '()))

      (test-case "Tree of item with no children or dependencies gives just the item"
        (check-equal? (cmd:tree st1 #:filter '(item . 3) #:post-filter '()) '((3))))

      (test-case "Tree of item with one dependency"
        (check-equal? (cmd:tree st1 #:filter '(item . 2) #:post-filter '()) '((2 (3)))))

      (test-case "Tree of multiple items gives all trees"
        (check-equal? (cmd:tree st1 #:filter '(or (item . 3) (item . 2)) #:post-filter '()) '((3) (2 (3)))))

      (test-case "Tree of item with both multiple levels of nesting"
        (check-equal? (cmd:tree st1 #:filter '(item . 1) #:post-filter '()) '((1 (2 (3))))))

      (test-case "Hiding children at first level of nesting using #:post-filter"
        (check-equal? (cmd:tree st1 #:filter '(item . 1) #:post-filter '(not (item . 2))) '((1))))

      (test-case "Hiding children at second level of nesting using #:post-filter"
        (check-equal? (cmd:tree st1 #:filter '(item . 1) #:post-filter '(not (item . 3))) '((1 (2)))))

      (test-case "Tree of item with child link and multiple levels of nesting"
        (check-equal? (cmd:tree st1 #:filter '(item . 5) #:post-filter '()) '((5 (1 (2 (3)))))))

      (test-case "Tree with ellipses"
        (check-equal? (cmd:tree st1 #:filter '(item . 4) #:post-filter '()) '((4 (2 (3)) (1 (2 ()))))))

      (test-case "Multiple trees with ellipses caused by previous tree"
        (check-equal? (cmd:tree st1 #:filter '(or (item . 1) (item . 4)) #:post-filter '()) '((1 (2 (3))) (4 (2 ()) (1 (2 ()))))))

      (test-case "Using more advanced #:post-filter to filter trees"
        ;; TODO: We shouldn't require this order.
        (check-equal? (cmd:tree st1 #:filter '() #:post-filter '(not (status : (string . "inactive")))) '((1) (3) (2 (3)) (5) (4)))))

    (test-suite "Agenda"
      (test-case "Filter by date with single item"
        (check-equal? (cmd:agenda st2 #:filter '(date : (date 2019 10 27))) `((,(dt:datetime 2019 10 27) 4))))

      (test-case "Multiple items are sorted by urgency"
        (check-equal? (cmd:agenda st2 #:filter '(date : (date 2019 10 23))) `((,(dt:datetime 2019 10 23) 2 5 3))))

      (test-case "Showing multiple dates"
        (check-equal? (cmd:agenda st2 #:filter '(or (date : (date 2019 10 23))
                                                    (date : (date 2019 10 27))))
                      `((,(dt:datetime 2019 10 23) 2 5 3)
                        (,(dt:datetime 2019 10 27) 4))))

      (test-case "Filter matching nothing gives empty agenda"
        (check-equal? (cmd:agenda st2 #:filter '(not ()))
                      '()))

      (test-case "Showing everything does not include items with no date"
        (check-equal? (cmd:agenda st2 #:filter '())
                      `((,(dt:datetime 2019 10 23) 2 5 3)
                        (,(dt:datetime 2019 10 27) 4)))))

    (test-suite "Add"
      (let-values (((st* item*) (cmd:add st3 #:modify '(and (description : (string . "Item 6"))
                                                            (tags + (string . "tag"))
                                                            (tags + (string . "new-tag"))))))
        (define item-state (a:get-path (st* s:state.item-state)))

        (test-case "Added item has expected ID"
          (check-equal? item* 6))

        (test-case "Added item with properties set is found in new state"
          (check-true (i:found? item-state item*)))

        (test-case "Added item has correct properties set"
          (check-equal? (i:get item-state item* 'description) (v:make-string "Item 6"))
          (check-equal? (i:get item-state item* 'tags) (v:make-set (set (v:make-string "tag") (v:make-string "new-tag"))))))

      (test-case "Adding item takes into account modify contexts"
        (let-values (((st* item*) (~> st3
                                      (a:update s:state.context-state (λ> (c:activate "tagged")))
                                      (cmd:add #:modify '(and (description : (string . "Item 6"))
                                                              (tags + (string . "tag"))
                                                              (tags + (string . "new-tag")))))))
          (define item-state (a:get-path (st* s:state.item-state)))
          (check-equal? (i:get item-state item* 'description) (v:make-string "Item 6"))
          (check-equal? (i:get item-state item* 'tags) (v:make-set (set (v:make-string "tag") (v:make-string "new-tag") (v:make-string "other-tag")))))))))
