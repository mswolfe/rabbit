(module rabbit racket
  (require racket/date
           "rabbit-db.rkt")
  
  (provide start-task
           stop-task
           get-current-task
           get-tasks/today
           get-tasks/week
           get-tasks/month
           get-category-totals/today
           get-category-totals/week
           get-category-totals/month
           get-total
           (struct-out total))
  
  ; A struct to hold the total time for a category.
  (struct total (id name time)
    #:transparent)
  
  ; Starts a task for this category.  This
  ; will stop any currently active task for the
  ; same category.  Returns the task-id for
  ; newly created task.
  ;
  ; db? string? -> exact-integer?
  (define (start-task db category)
    (stop-task db category)
    (create-task db category))
  
  ; Stops the currently active task for the given
  ; category by setting the stop time for that task.
  ;
  ; db? string? -> boolean?
  (define (stop-task db category)
    (let ((task-id (get-task-id/stop db category 0)))
      (if (= task-id -1)
          #f
          (update-task-stop db task-id))))
  
  ; Returns the currently active task for this category.
  ;
  ; db? string? -> or task? boolean?
  (define (get-current-task db category)
    (let ((task-id (get-task-id/stop db category 0)))
      (if (= task-id -1)
          #f
          (get-task db task-id))))
  
  ; Returns all tasks that occured today for the given category.
  ;
  ; db? string? -> listof task?
  (define (get-tasks/today db [category #f])
    (let* ((now (current-date))
           (today (+
                   (* (date-hour now) 60 60 1000)
                   (* (date-minute now) 60 1000)
                   (* (date-second now) 1000)))
           (start (- (current-milliseconds) today)))
      (if (boolean? category)
          (get-tasks db start)
          (get-tasks/category db category start))))
  
  ; Returns the total amount of time spent on all tasks
  ; for each category that occured today.
  ;
  ; db? string? -> listof total?
  (define (get-category-totals/today db [category #f])
    (let ((tasks (get-tasks/today db category)))
      (append (get-category-totals tasks) (list (get-total tasks)))))
  
  ; Returns all tasks that occured this week for the given category.
  ;
  ; db? string? -> listof task?
  (define (get-tasks/week db [category #f])
    (let* ((now (current-date))
           (today (+
                   (* (date-hour now) 60 60 1000)
                   (* (date-minute now) 60 1000)
                   (* (date-second now) 1000)
                   (* (date-week-day now) * 24 * 60 * 60 * 1000)))
           (start (- (current-milliseconds) today)))
      (if (boolean? category)
          (get-tasks db start)
          (get-tasks/category db category start))))
  
  ; Returns the total amount of time spent on all tasks
  ; for each category that occured this week.
  ;
  ; db? string? -> listof total?
  (define (get-category-totals/week db [category #f])
    (get-category-totals (get-tasks/week db category)))
  
  ; Returns all tasks that occured this month for the given category.
  ;
  ; db? string? -> listof task?
  (define (get-tasks/month db [category #f])
    (let* ((now (current-date))
           (today (+
                   (* (date-hour now) 60 60 1000)
                   (* (date-minute now) 60 1000)
                   (* (date-second now) 1000)
                   (* (date-day now) * 24 * 60 * 60 * 1000)))
           (start (- (current-milliseconds) today)))
      (if (boolean? category)
          (get-tasks db start)
          (get-tasks/category db category start))))
  
  ; Returns the total amount of time spent on all tasks
  ; for each category that occured this month.
  ;
  ; db? string? -> listof total?
  (define (get-category-totals/month db [category #f])
    (get-category-totals (get-tasks/month db category)))
  
  ; Calculates the totals for each category given the
  ; list of tasks.
  ;
  ; listof task? -> listof total?
  (define (get-category-totals tasks)
    (foldl (lambda (task result)
               ; Find the total structure in result which is holding the total for
               ; this category.
               (let* ((id (task-category-id task))
                      (time (if (= (task-stop task) 0)
                                (- (current-milliseconds) (task-start task))
                                (- (task-stop task) (task-start task))))
                      (name (task-category-name task))
                      (t (findf (lambda (t) (= (total-id t) id)) result)))
                 ; If we find it, return a new list with the updated total, otherwise
                 ; create a new entry in the list.
                 (if (total? t)
                     (append (remove t result) (list (total id name (+ (total-time t) time))))
                     (append result (list (total id name time))))))
             '()
             tasks))
  
  (define (get-total tasks)
    (foldl (lambda (task result)
             (let ((time (if (= (task-stop task) 0)
                                (- (current-milliseconds) (task-start task))
                                (- (task-stop task) (task-start task)))))
               (+ result time)))
           0
           tasks))
  )
  
