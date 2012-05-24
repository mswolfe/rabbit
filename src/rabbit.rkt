(module rabbit racket
  (require racket/date
           "rabbit-common.rkt"
           "rabbit-db.rkt")
  
  (provide add-category
           change-category
           remove-category
           list-categories
           
           begin-task
           end-task
           end-tasks
           switch-task
           list-active-tasks
           remove-task
           
           day
           week
           month
           year
           list-tasks
           
           list-task-totals
           list-category-totals)
  
  ; Creates a category with the given name
  ; if the category name doesn't currently
  ; exist.  Returns the category id assigned
  ; to the new category or #false when creation
  ; fails.
  ;
  ; db? string? -> (or exact-integer? #f)
  (define (add-category db category)
    (if (= (select-category-id db category) -1)
        (insert-category db category)
        #f))
  
  ; Updates the old category name to the new category
  ; name.  Returns true when the update is successful,
  ; false otherwise.  If the new category name is a duplicate
  ; it will not be allowed to update.
  ;
  ; db? string? string? -> boolean?
  (define (change-category db old-category new-category)
    (let ((old-cid (select-category-id db old-category))
          (new-cid (select-category-id db new-category)))
      ; When the old-cid is a valid category and the new
      ; cid indicates that we have not match for the string
      ; then update the old category to the new name.
      (if (and (not (= old-cid -1))
               (= new-cid -1))
          (update-category db old-cid new-category)
          #f)))
  
  ; Removes the category from the database.  This will
  ; only be allowed if their are no tasks associated with
  ; this category.
  ;
  ; db? string? -> boolean?
  (define (remove-category db category)
    (let ((cid (select-category-id db category)))
      (if (= cid -1)
          ; No category!
          #f
          ; Check to make sure we don't have associated tasks.
          (if (empty? (select-task-ids db #:cid cid))
              (delete-category db cid)
              #f))))
  
  ; Returns a list of categories.
  ;
  ; db? -> (listof categories)
  (define (list-categories db)
    (select-categories db))
  
  ; Begins a task for this category.
  ; Returns the task-id for newly created task.
  ; Creates the category if needed.
  ;
  ; db? string? -> exact-integer?
  (define (begin-task db category)
    (let ((cid (select-category-id db category))
          (start (current-milliseconds))
          (stop 0))
      (if (= cid -1)
          (insert-task db (insert-category db category) start stop)
          (insert-task db cid start stop))))
  
  ; Ends the task for the given task id which means setting the stop
  ; time of the task to the current time in milliseconds.
  ;
  ; db? exact-integer? -> boolean?
  (define (end-task db tid)
    (update-task db tid #:stop (current-milliseconds)))
  
  ; Ends all tasks that have a stop time of zero.  When the category
  ; is given only tasks within that category will be stopped, otherwise
  ; all tasks with a stop time of zero will be updated such that their
  ; stop time will reflect current time.
  ;
  ; db? string? -> boolean?
  (define (end-tasks db #:category [category #f])
    (let ((stop (current-milliseconds)))
      ; Make sure we have a category as a string.
      (if (string? category)
          ; Find the cid and update all tasks with that cid
          ; and a stop time of zero to a current stop time. 
          (let ((cid (select-category-id db category)))
            (if (= cid -1)
                #f
                (update-task/stop db 0 stop #:cid cid)))
          ; Not a valid category?
          (update-task/stop db 0 stop))))
    
  
  ; Switches the current working task which means ending all tasks for
  ; any category and starting a task for the given category.
  ;
  ; db? string? -> exact-integer?
  (define (switch-task db category)
    (end-tasks db)
    (begin-task db category))
  
  ; Returns the active tasks and filters on the given category if requested.
  ;
  ; db? string? -> (listof task?)
  (define (list-active-tasks db #:category [category #f])
    (if (string? category)
        (let ((cid (select-category-id db category)))
          (if (= cid -1)
              '()
              (select-tasks db #:cid cid #:stop 0)))
        (select-tasks db #:stop 0)))
  
  ; Removes the task from the database.
  ;
  ; db? exact-integer? -> boolean?
  (define (remove-task db tid)
    (delete-task db tid))
  
  ; Returns the amount of time in ms that has occured
  ; during this day.
  ;
  ; void -> exact-integer?
  (define (day [now (current-date)])
    (let ((time (+
                  (* (date-hour now) 60 60 1000)
                  (* (date-minute now) 60 1000)
                  (* (date-second now) 1000))))
      (- (current-milliseconds) time)))
  
  ; Returns the amount of time in ms that has occured
  ; during this week.
  ;
  ; void -> exact-integer?
  (define (week [now (current-date)])
    (let ((time (+
                 (day now)
                 (* (date-week-day now) 24 60 60 1000))))
      (- (current-milliseconds) time)))
  
  ; Returns the amount of time in ms that has occured
  ; during this month.
  ;
  ; void -> exact-integer?
  (define (month [now (current-date)])
     (let ((time (+
                   (day now)
                   (* (date-day now) 24 60 60 1000))))
       (- (current-milliseconds) time)))
  
  ; Returns the amount of time in ms that has occured
  ; during this year.
  ;
  ; void -> exact-integer?
  (define (year [now (current-date)])
     (let ((time (+
                   (day now)
                   (* (date-year-day now) 24 60 60 1000))))
       (- (current-milliseconds) time)))
  
  ; Returns all tasks that lie between the given start and stop time and
  ; filtered for the category if the category exists.
  ; 
  ; db? (or procedure? exact-integer?) (or procedure? exact-integer?) string? -> (listof task?)
  (define (list-tasks db start stop #:category [category #f])
    (let ((start-time (if (procedure? start) (start) start))
          (stop-time (if (procedure? stop) (stop) stop)))
      (if (string? category)
          (let ((cid (select-category-id db category)))
            (if (= cid -1)
                '()
                (select-tasks-range db start-time stop-time #:cid cid)))
          (select-tasks-range db start-time stop-time))))
  
  ; Returns the total amount of time spent for each task.
  ;
  ; (listof task?) -> (listof exact-integer?)
  (define (list-task-totals tasks)
    (map (lambda (t)
            (task-total t))
      tasks))
  
  ; Calculates the totals for each category given the
  ; list of tasks.
  ;
  ; listof task? -> listof total?
  (define (list-category-totals tasks)
    (foldl (lambda (task result)
               ; Find the total structure in result which is holding the total for
               ; this category.
               (let* ((cid (task-cid task))
                      (time (task-total task))
                      (name (task-name task))
                      (t (findf (lambda (t) (= (total-cid t) cid)) result)))
                 ; If we find it, return a new list with the updated total, otherwise
                 ; create a new entry in the list.
                 (if (total? t)
                     (append (remove t result) (list (total cid name (+ (total-time t) time))))
                     (append result (list (total cid name time))))))
             '()
             tasks))
  )
  
