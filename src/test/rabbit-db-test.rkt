#lang racket
(require rackunit
         (planet jaymccarthy/sqlite:5:1/sqlite)
         "../rabbit-db.rkt")

(provide rabbit-db-tests)
  
(define test-db "/tmp/test-db")

(define (clean)
  (when (file-exists? test-db)
    (delete-file test-db)))

(define rabbit-db-tests
  (test-suite
   "All tests for rabbit-db"
   #:before clean
   #:after clean
   
   (test-case
    "Open/Close"
    (let ((db (open-db test-db)))
      (check-true (db? db))
      
      (close-db db)
      (clean)))
  
   (test-case
    "Category"
    (let ((db (open-db test-db)))
      (check-equal? (get-category-id db "work") -1)
      
      (check-equal? (get-category-id/new db "work") 1)
      (check-equal? (get-category-id db "work") 1)
      
      (check-equal? (get-category-id/new db "play") 2)
      (check-equal? (get-category-id db "play") 2)
      
      (check-equal? (get-category-id db "bad") -1)
      
      (check-false (update-category db "bad" "false"))
      
      (check-true (update-category db "work" "work2"))
      (check-true (update-category db "work2" "work3"))
      (check-equal? (get-category-id db "work3") 1)
      
      (check-false (update-category db "work" "false"))
      (check-false (update-category db "work2" "false"))
      
      (check-false (delete-category db "false"))
      
      (check-true (delete-category db "play"))
      (check-false (delete-category db "play"))
      
      (exec/ignore db "insert into tasks (category_id,start,stop) values (1,5,10);")
      (check-false (delete-category db "work3"))
      
      (close-db db)
      (clean)))
   
   (test-case
    "Task"
    (let ((db (open-db test-db)))
      (check-equal? (create-task db "work") 1)
      
      (check-false (update-task-category db 5 "play"))
      (check-true (update-task-category db 1 "play"))
      
      (check-false (update-task-start db 5 10))
      (check-true (update-task-start db 1 10))
      
      (check-false (update-task-stop db 5 20))
      (check-true (update-task-stop db 1 20))
      
      (check-false (get-task db 5))
      (check-equal? (get-task db 1)
                    (task 1 2 10 20 "play"))
      
      (check-equal? (create-task db "work") 2)
      (check-equal? (create-task db "work") 3)
      (check-equal? (create-task db "play") 4)
      (check-equal? (create-task db "play") 5)
      
      (check-equal? (length (get-tasks/category db "work")) 2)
      (check-equal? (length (get-tasks/category db "play")) 3)
      (check-true (empty? (get-tasks/category db "false")))
      
      (check-equal? (get-task-id/start db "false" 5) -1)
      (check-equal? (get-task-id/start db "play" 5) -1)
      (check-equal? (get-task-id/start db "play" 10) 1)
      
      (check-equal? (get-task-id/stop db "false" 5) -1)
      (check-equal? (get-task-id/stop db "play" 5) -1)
      (check-equal? (get-task-id/stop db "play" 20) 1)
      
      
      (check-false (delete-task db 55))
      (check-true (delete-task db 2))
      (check-true (delete-task db 4))
      
      (check-equal? (length (get-tasks/category db "work")) 1)
      (check-equal? (length (get-tasks/category db "play")) 2)
      
      (close-db db)
      (clean)))
   
   ))
  