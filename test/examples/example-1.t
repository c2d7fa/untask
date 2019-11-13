((version 0)
 (next-item-id 4)
 (active-contexts ("test"))
 (all-contexts (("test" ((or (and (tags - (string "tag")))) ()))))
 (item-property-data
  ((1 ((description (string "Task 1"))))
   (3 ((description (string "Task 3")) (tags (set ((string "tag"))))))
   (2
    ((description (string "Task 2"))
     (status (string "active"))
     (tags (set ((string "tag"))))
     (depends (set ((item 3)))))))))
