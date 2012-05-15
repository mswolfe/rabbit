(module rabbit-db racket
  (require (planet jaymccarthy/sqlite:5:1/sqlite))
  
  (provide open-db
           close-db
           
           get-category-id
           get-category-id/new
           update-category
           delete-category
           
           create-task
           update-task-category
           update-task-start
           update-task-stop
           delete-task
           
           get-task-id/start
           get-task-id/stop
           get-task
           get-tasks
           get-tasks/category
           (struct-out task))
  
  (define CREATE_CATEGORIES "create table if not exists categories (category_id INTEGER PRIMARY KEY, name TEXT);")
  (define CREATE_TASKS "create table if not exists tasks (task_id INTEGER PRIMARY KEY, category_id INTEGER SECONDARY KEY, start INTEGER, stop INTEGER);")
  
  ; Initializes the database by creating both the
  ; categories and tasks table if needed.
  ;
  ; string? -> or db? #f
  (define (open-db db-string)
    (let ((db (open (string->path db-string))))
      (if (db? db)
          (begin
            (exec/ignore db CREATE_CATEGORIES)
            (exec/ignore db CREATE_TASKS)
            db)
          #f)))
  
  ; Closes the given database.
  ;
  ; db? -> void
  (define (close-db db)
    (close db))
  
  (define INSERT_CATEGORY "insert into categories (name) values (?);")
  
  ; Returns the category id for the given category name.
  ; Creates a new one if needed.
  ;
  ; db? string? -> exact-integer?
  (define (get-category-id/new db category)
    (let ((category-id (get-category-id db category)))
      (if (= category-id -1)
          (insert db INSERT_CATEGORY category)
          category-id)))
  
  (define SELECT_CATEGORY "select category_id from categories where name=?")
  
  ; Returns the category id for the given category name
  ; or -1 when it doesn't exist.
  ;
  ; db? string? -> exact-integer?
  (define (get-category-id db category)
    (let ((data (select db SELECT_CATEGORY category)))
      (if (empty? data)
          -1
          (vector-ref (second data) 0))))
  
  (define UPDATE_CATEGORY "update categories SET name = ? where category_id = ?;")
  
  ; Updates the given category name to the new category name.
  ;
  ; db? string? string? -> boolean?
  (define (update-category db category-old category-new)
    (let ((category-id (get-category-id db category-old)))
      (if (= category-id -1)
          #f
          (begin
            (exec/ignore db UPDATE_CATEGORY category-new category-id)
            (= (changes-count db) 1)))))
  
  (define SEARCH_TASK_CATEGORY "select count(category_id) from tasks where category_id=?;")
  (define DELETE_CATEGORY "delete from categories where category_id=?;")
  
  ; Removes the category from the database.  This will only
  ; be allowed when there are no tasks referencing this category.
  ;
  ; db? string? -> boolean?
  (define (delete-category db category)
    (let ((category-id (get-category-id db category)))
      (if (= category-id -1)
          #f
          (let ((num-rows 0))
            (exec db SEARCH_TASK_CATEGORY
                  (lambda (names values)
                    (set! num-rows (vector-ref values 0))
                    0)
                  category-id)
            (if (= num-rows 0)
                (begin
                  (exec/ignore db DELETE_CATEGORY category-id)
                  (= (changes-count db) 1))
                #f)))))
  
  (define INSERT_TASK "insert into tasks (category_id, start, stop) values (?,?,?);")
  
  ; Inserts a new entry into the tasks table using
  ; the given category (creating the category if needed)
  ; and using the current timestamp.
  ;
  ; db? string? exact-integer? exact-integer? -> exact-integer?
  (define (create-task db category [start (current-milliseconds)] [stop 0])
    (let ((category-id (get-category-id/new db category)))
      (insert db INSERT_TASK category-id start stop)))
  
  (define UPDATE_TASK_CATEGORY "update tasks set category_id = ? where task_id = ?;")
  
  ; Updates the category for the given task id.  Creates
  ; a new category if needed.
  ;
  ; db? exact-integer? string? -> boolean?
  (define (update-task-category db task-id category)
    (let ((category-id (get-category-id/new db category)))
      (exec/ignore db UPDATE_TASK_CATEGORY category-id task-id)
      (= (changes-count db) 1)))
  
  (define UPDATE_TASK_START "update tasks set start = ? where task_id = ?;")
  
  ; Updates the start time for the given task id.
  ;
  ; db? exact-integer? exact-integer? -> boolean?
  (define (update-task-start db task-id [start (current-milliseconds)])
    (exec/ignore db UPDATE_TASK_START start task-id)
    (= (changes-count db) 1))
  
  (define UPDATE_TASK_STOP "update tasks set stop = ? where task_id = ?;")
  
  ; Updates the stop time for the given task id.
  ;
  ; db? exact-integer? exact-integer? -> boolean?
  (define (update-task-stop db task-id [stop (current-milliseconds)])
    (exec/ignore db UPDATE_TASK_STOP stop task-id)
    (= (changes-count db) 1))
  
  (define DELETE_TASK "delete from tasks where task_id=?;")
  
  ; Removes the task from the database.
  ;
  ; db? exact-integer? -> boolean?
  (define (delete-task db task-id)
    (exec/ignore db DELETE_TASK task-id)
    (= (changes-count db) 1))
  
  ; The task structure
  ;
  ; task-id : exact-integer?
  ; category-id : exact-integer?
  ; category-name : string?
  ; start : exact-integer?
  ; stop : exact-integer?
  ; category-created : exact-integer?
  (struct task (task-id category-id start stop category-name)
    #:transparent)
  
  (define SELECT_TASK "select distinct * from tasks natural join categories where task_id=?;")
  
  ; Returns the task associated with the given task id.
  ;
  ; db? exact-integer? -> or task? boolean?
  (define (get-task db task-id)
    (let ((data (select db SELECT_TASK task-id)))
      (if (empty? data)
          #f
          (let ((td (second data)))
            (task (vector-ref td 0)
                  (vector-ref td 1)
                  (vector-ref td 2)
                  (vector-ref td 3)
                  (vector-ref td 4))))))
  
  (define SEARCH_TASK_START "select task_id from tasks where category_id=? and start=?;")
  
  ; Returns the task id associated with the given category and start time.
  ;
  ; db? string? exact-integer? -> exact-integer?
  (define (get-task-id/start db category start)
    (let ((category-id (get-category-id db category)))
      (if (= category-id -1)
          -1
          (let ((data (select db SEARCH_TASK_START category-id start)))
            (if (empty? data)
                -1
                (vector-ref (second data) 0))))))
  
  (define SEARCH_TASK_STOP "select task_id from tasks where category_id=? and stop=?;")
  
  ; Returns the task id associated with the given category and stop time.
  ;
  ; db? string? exact-integer? -> exact-integer?
  (define (get-task-id/stop db category stop)
    (let ((category-id (get-category-id db category)))
      (if (= category-id -1)
          -1
          (let ((data (select db SEARCH_TASK_STOP category-id stop)))
            (if (empty? data)
                -1
                (vector-ref (second data) 0))))))
  
  (define SELECT_TASKS "select distinct * from tasks natural join categories where category_id=? and start >= ? and stop <= ?;")
  
  ; Returns the tasks associated with the given category that lie between
  ; the indicated start and stop times.
  ;
  ; db? string? exact-integer? exact-integer? -> or listof task? boolean?
  (define (get-tasks/category db category [start 0] [stop (current-milliseconds)])
    (let ((category-id (get-category-id db category)))
      (if (= category-id -1)
          '()
          (let ((data (select db SELECT_TASKS category-id start stop)))
            (if (empty? data)
                '()
                (let ((create-task 
                       (lambda (td)
                         (task (vector-ref td 0)
                               (vector-ref td 1)
                               (vector-ref td 2)
                               (vector-ref td 3)
                               (vector-ref td 4)))))
                  (map create-task (rest data))))))))
  
  (define SELECT_ALL_TASKS "select distinct * from tasks natural join categories where start >= ? and stop <= ?;")
  
  ; Returns the tasks that lie between the indicated start and stop times.
  ;
  ; db? exact-integer? exact-integer? -> or listof task? boolean?
  (define (get-tasks db [start 0] [stop (current-milliseconds)])
    (let ((data (select db SELECT_ALL_TASKS start stop)))
            (if (empty? data)
                '()
                (let ((create-task 
                       (lambda (td)
                         (task (vector-ref td 0)
                               (vector-ref td 1)
                               (vector-ref td 2)
                               (vector-ref td 3)
                               (vector-ref td 4)))))
                  (map create-task (rest data))))))
  )