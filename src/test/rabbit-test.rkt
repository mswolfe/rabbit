#lang racket
(require rackunit
         (planet jaymccarthy/sqlite:5:1/sqlite)
         
         "../rabbit-common.rkt"
         "../rabbit-db.rkt"
         "../rabbit.rkt")

(provide rabbit-tests)
  
(define test-db "/tmp/test-db")

(define (clean)
  (when (file-exists? test-db)
    (delete-file test-db)))

(define rabbit-tests
  (test-suite
   "All tests for rabbit"
   #:before clean
   #:after clean
  
   (test-case
    "Category"
    (let ((db (open-db test-db)))
           
      ; Check add
      (check-equal? (add-category db "work") 1)
      (check-equal? (add-category db "play") 2)
      (check-equal? (add-category db "work") #f)
      (check-equal? (add-category db "play") #f)
      (check-equal? (add-category db "chores") 3)
      
      ; Check update
      (check-true (change-category db "work" "work2"))
      (check-false (change-category db "play" "work2"))
      (check-true (change-category db "work2" "work"))
      
      ; Check list
      (check-equal? (list-categories db)
                    (list (category 1 "WORK")
                          (category 2 "PLAY")
                          (category 3 "CHORES")))
      
      ; Check remove
      (check-true (remove-category db "work"))
      (check-false (remove-category db "false"))
      
      (check-equal? (insert-task db 2 1000 2000) 1)
      (check-false (remove-category db "play"))
      
      (close-db db)
      (clean)))
   
   (test-case
    "Tasks"
    (let ((db (open-db test-db)))
      (add-category db "work")   ; 1
      (add-category db "play")   ; 2
           
      ; Check begin
      (check-equal? (begin-task db "work") 1)
      (check-equal? (begin-task db "play") 2)
      (check-equal? (begin-task db "chores") 3) ; category id = 3
      
      ; Check end
      (check-true (end-task db 1))
      (check-false (equal? (task-stop (select-task db #:tid 1)) 0))
      
      ; Check end tasks
      (check-true (end-tasks db))
      (check-false (equal? (task-stop (select-task db #:tid 2)) 0))
      (check-false (equal? (task-stop (select-task db #:tid 3)) 0))
      
      (check-equal? (begin-task db "work") 4)
      (check-equal? (begin-task db "chores") 5)
      (check-true (end-tasks db #:category "work"))
      (check-false (equal? (task-stop (select-task db #:tid 4)) 0))
      (check-true (equal? (task-stop (select-task db #:tid 5)) 0))
      
      ; Check switch task
      (check-equal? (begin-task db "work") 6)
      (check-equal? (switch-task db "play") 7) ; task-id 7
      (check-false (equal? (task-stop (select-task db #:tid 6)) 0))
      (check-true (equal? (task-stop (select-task db #:tid 7)) 0))
      
      (check-equal? (switch-task db "play") 8) ; task-id 8
      (check-false (equal? (task-stop (select-task db #:tid 7)) 0))
      (check-true (equal? (task-stop (select-task db #:tid 8)) 0))
      
      (check-equal? (length (list-tasks db 0 (current-milliseconds))) 8)
      (check-equal? (length (list-tasks db 0 (current-milliseconds) #:category "work")) 3)
      (check-equal? (length (list-active-tasks db)) 1)
      (check-equal? (length (list-active-tasks db #:category "work")) 0)
      
      ; Check remove task
      (check-true (remove-task db 1))
      (check-true (remove-task db 2))
      (check-true (remove-task db 3))
      (check-true (remove-task db 4))
      (check-true (remove-task db 5))
      (check-true (remove-task db 6))
      (check-true (remove-task db 7))
      (check-true (remove-task db 8))
      
      ; Check list-task-totals
      (insert-task db 1 0 10)
      (insert-task db 1 10 25)
      (insert-task db 2 20 30)
      (check-equal? (list-task-totals (list-tasks db 0 30))
                    (list 10 15 10))
      (check-equal? (list-task-totals (list-tasks db 0 25))
                    (list 10 15))
      (check-equal? (list-task-totals (list-tasks db 0 30 #:category "work"))
                    (list 10 15))
      
      ; check list-category-totals
      (check-equal? (list-category-totals (list-tasks db 0 30))
                    (list
                      (total 1 "WORK" 25)
                      (total 2 "PLAY" 10)))
      (check-equal? (list-category-totals (list-tasks db 0 20))
                    (list
                      (total 1 "WORK" 10)))
      (check-equal? (list-category-totals (list-tasks db 0 30 #:category "work"))
                    (list
                      (total 1 "WORK" 25)))
      
      (close-db db)
      (clean)))
   
   ))
  