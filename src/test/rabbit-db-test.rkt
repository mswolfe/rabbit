#lang racket
(require rackunit
         (planet jaymccarthy/sqlite:5:1/sqlite)
         
         "../rabbit-common.rkt"
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
      ; Check insertion
      (check-equal? (insert-category db "work") 1)
      (check-equal? (insert-category db "play") 2)
      (check-equal? (insert-category db "chores") 3)
      
      ; Check selection
      (check-equal? (select-category-id db "work") 1)
      (check-equal? (select-category-id db "play") 2)
      (check-equal? (select-category-id db "chores") 3)
      (check-equal? (select-categories db)
                    (list (category 1 "WORK")
                          (category 2 "PLAY")
                          (category 3 "CHORES")))
      
      ; Check updating
      (check-true (update-category db 1 "work2"))
      (check-equal? (select-category-id db "work") -1)
      (check-equal? (select-category-id db "work2") 1)
      
      ; Check deletion
      (check-true (delete-category db 1))
      (check-equal? (select-category-id db "work") -1)
      (check-equal? (select-category-id db "work2") -1)
      
      (close-db db)
      (clean)))
   
   (test-case
    "Task"
    (let ((db (open-db test-db)))
      (check-equal? (insert-category db "work") 1)
      (check-equal? (insert-category db "play") 2)
      (check-equal? (insert-category db "chores") 3)
      
      ; Check insertion
      (check-equal? (insert-task db 1 1000 2000) 1)
      (check-equal? (insert-task db 2 2000 3000) 2)
      
      ; Check updating
      (check-true (update-task db 1 #:cid 2))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-cid t) 2))))
      
      (check-true (update-task db 1 #:start 5000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-start t) 5000))))
      
      (check-true (update-task db 1 #:stop 6000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-stop t) 6000))))
      
      (check-true (update-task db 1 #:cid 3 #:start 4000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-cid t) 3)
                         (= (task-start t) 4000))))
      
      (check-true (update-task db 1 #:cid 2 #:stop 7000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-cid t) 2)
                         (= (task-stop t) 7000))))
      
      (check-true (update-task db 1 #:start 3000 #:stop 8000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-start t) 3000)
                         (= (task-stop t) 8000))))
      
      (check-true (update-task db 1 #:cid 1 #:start 1000 #:stop 2000))
      (check-true (let ((t (select-task db #:tid 1)))
                    (and (task? t)
                         (= (task-cid t) 1)
                         (= (task-start t) 1000)
                         (= (task-stop t) 2000))))
      
      ; Check Deletion
      (check-true (delete-task db 2))
      (check-false (select-task db #:tid 2))
      
      ; Check select-task-id
      (check-equal? (insert-task db 1 4000 5000) 2)
      (check-equal? (insert-task db 2 6000 7000) 3)
      
      (check-equal? (select-task-id db #:cid 1) 1)
      (check-equal? (select-task-id db #:start 4000) 2)
      (check-equal? (select-task-id db #:stop 5000) 2)
      (check-equal? (select-task-id db #:cid 1 #:start 4000) 2)
      (check-equal? (select-task-id db #:cid 1 #:stop 5000) 2)
      (check-equal? (select-task-id db #:start 4000 #:stop 5000) 2)
      (check-equal? (select-task-id db #:cid 1 #:start 4000 #:stop 5000) 2)
      
      (check-equal? (select-task-id db #:cid 8) -1)
      (check-equal? (select-task-id db #:start 10000) -1)
      (check-equal? (select-task-id db #:stop 5) -1)
      
      ; Check select-task-ids
      (check-equal? (select-task-ids db #:cid 1)
                    (list 1 2))
      (check-equal? (select-task-ids db #:cid 2)
                    (list 3))
      
      ; Check select-task
      (check-equal? (select-task db #:tid 3)
                    (task 3 2 "PLAY" 6000 7000))
      (check-equal? (select-task db #:cid 2 #:start 6000)
                    (task 3 2 "PLAY" 6000 7000))
      
      ; Check select-tasks
      (check-equal? (select-tasks-range db 4000 7000)
                    (list
                     (task 2 1 "WORK" 4000 5000)
                     (task 3 2 "PLAY" 6000 7000)))
      
      (check-equal? (select-tasks-range db 500 6000 #:cid 1)
                    (list
                     (task 1 1 "WORK" 1000 2000)
                     (task 2 1 "WORK" 4000 5000)))
      
      ; Check update-task/stop
      (check-equal? (insert-task db 1 10000 0) 4)
      (check-equal? (insert-task db 2 20000 0) 5)
      
      (check-true (update-task/stop db 0 55))
      (check-equal? (task-stop (select-task db #:tid 4))
                    55)
      (check-equal? (task-stop (select-task db #:tid 5))
                    55)
      
      (check-true (update-task/stop db 55 88 #:cid 1))
      (check-equal? (task-stop (select-task db #:tid 4))
                    88)
      (check-equal? (task-stop (select-task db #:tid 5))
                    55)
      
      (close-db db)
      (clean)))
   
   ))
  