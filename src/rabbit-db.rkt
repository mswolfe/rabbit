(module rabbit-db racket
  (require (planet jaymccarthy/sqlite:5:1/sqlite)
           
           "rabbit-common.rkt")
  
  (provide open-db
           close-db
           
           insert-category
           update-category
           delete-category
           select-category-id
           select-categories
           
           insert-task
           update-task
           update-task/stop
           delete-task
           select-task-id
           select-task-ids
           select-task
           select-tasks
           select-tasks-range)
  
  (define CREATE_CATEGORIES "create table if not exists categories (category_id INTEGER PRIMARY KEY, name TEXT);")
  (define CREATE_TASKS "create table if not exists tasks (task_id INTEGER PRIMARY KEY, category_id INTEGER SECONDARY KEY, start INTEGER, stop INTEGER);")
  
  ; Initializes the database by creating both the
  ; categories and tasks table if needed.
  ;
  ; string? -> (or db? #f)
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
  
  ; Creates an entry in the categories table with the
  ; given string in upper case.  Returns the identifier
  ; given to the entry.
  ;
  ; db? string? -> exact-integer?
  (define (insert-category db category)
    (insert db INSERT_CATEGORY (string-upcase category)))
  
  (define UPDATE_CATEGORY "update categories SET name = ? where category_id = ?;")
  
  ; Updates the given category id with the new category name.
  ;
  ; db? exact-integer? string? -> boolean?
  (define (update-category db cid category-new)
    (exec/ignore db UPDATE_CATEGORY (string-upcase category-new) cid)
    (= (changes-count db) 1))
  
  (define DELETE_CATEGORY "delete from categories where category_id=?;")
  
  ; Removes the category from the database.  Note that this can
  ; be dangerous because a task may reference this category id.!
  ;
  ; db? string? -> boolean?
  (define (delete-category db cid)
    (exec/ignore db DELETE_CATEGORY cid)
    (= (changes-count db) 1))
  
  (define SELECT_CATEGORY_ID "select category_id from categories where name=?")
  
  ; Returns the category id for the given category name
  ; or -1 when it doesn't exist.
  ;
  ; db? string? -> exact-integer?
  (define (select-category-id db category)
    (let ((data (select db SELECT_CATEGORY_ID (string-upcase category))))
      (if (empty? data)
          -1
          (vector-ref (second data) 0))))
  
  (define SELECT_CATEGORY "select category_id,name from categories;")
  
  ; Returns the category id and category name for
  ; all entries in the database.  Returns a list of 
  ; categories.
  ;
  ; db? string? -> (listof category?)
  (define (select-categories db)
    (let ((data (select db SELECT_CATEGORY)))
      (if (empty? data)
          '()
          (map (lambda (row)
                 (category (vector-ref row 0)
                           (vector-ref row 1)))
               (rest data)))))
  
  (define INSERT_TASK "insert into tasks (category_id, start, stop) values (?,?,?);")
  
  ; Inserts a new entry into the tasks table using
  ; the given category id, start timestamp and stop timestamp.
  ; Returns the task identifier for the new entry.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> exact-integer?
  (define (insert-task db cid start stop)
    (insert db INSERT_TASK cid start stop))
  
  (define UPDATE_TASK_CATEGORY "update tasks set category_id = ? where ")
  (define UPDATE_TASK_START "update tasks set start = ? where ")
  (define UPDATE_TASK_STOP "update tasks set stop = ? where ")
  
  ; Updates the given values for the given task id.  Returns true when all three
  ; updates are taken, false otherwise.
  ;
  ; db? exact-integer? exact-integer? exact-integer? exact-integer? -> boolean?
  (define (update-task db
                       tid
                       #:cid [cid #f]
                       #:start [start #f]
                       #:stop [stop #f])
    (let ((start-changes (total-changes-count db))
          (num-changes (+ 
                        (if (boolean? cid) 0 1)
                        (if (boolean? start) 0 1)
                        (if (boolean? stop) 0 1))))
      (unless (boolean? cid)
        (exec/ignore db (generate-query UPDATE_TASK_CATEGORY tid #f #f #f) cid))
      (unless (boolean? start)
        (exec/ignore db (generate-query UPDATE_TASK_START tid #f #f #f) start))
      (unless (boolean? stop)
        (exec/ignore db (generate-query UPDATE_TASK_STOP tid #f #f #f) stop))
      (= (total-changes-count db) (+ start-changes num-changes))))
  
  (define UPDATE_ALL_TASK_STOP "update tasks set stop = ? where ")
  
  ; Updates the stop field for all task rows where the current stop value
  ; is equal to the given old-stop.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> boolean?
  (define (update-task/stop db old-stop new-stop #:cid [cid #f])
    (exec/ignore db (generate-query UPDATE_ALL_TASK_STOP #f cid #f old-stop) new-stop)
    (> (changes-count db) 0))
  
  (define DELETE_TASK "delete from tasks where task_id=?;")
  
  ; Removes the task from the database.
  ;
  ; db? exact-integer? -> boolean?
  (define (delete-task db tid)
    (exec/ignore db DELETE_TASK tid)
    (= (changes-count db) 1))
  
  (define SELECT_TASK_ID "select task_id from tasks where ")
  
  ; Returns the task id associated with the given category id,
  ; start time and stop time.  The values default to '*' in the
  ; query and if more then one result is returned only the first
  ; task id will be returned.  Returns a value of -1 when the task id
  ; cannot be found.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> exact-integer?
  (define (select-task-id db
                       #:cid [cid #f]
                       #:start [start #f]
                       #:stop [stop #f])
    (let ((data (select db (generate-query SELECT_TASK_ID #f cid start stop))))
      (if (empty? data)
          -1
          (vector-ref (second data) 0))))
  
  ; Returns the task ids associated with the given category id,
  ; start time and stop time.  The values default to '*' in the
  ; query and this function will always return a list.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> listof exact-integer?
  (define (select-task-ids db
                        #:cid [cid #f]
                        #:start [start #f]
                        #:stop [stop #f])
    (let ((data (select db (generate-query SELECT_TASK_ID #f cid start stop))))
      (if (empty? data)
          '()
          (flatten (map vector->list (rest data))))))
    
  (define SELECT_TASK "select distinct task_id, category_id, name, start, stop from tasks natural join categories where ")
     
  ; Returns the task found with the given parameters.
  ;
  ; db? exact-integer? exact-integer? exact-integer? exact-integer? -> or task? boolean?
  (define (select-task db
                       #:tid [tid #f]
                       #:cid [cid #f]
                       #:start [start #f]
                       #:stop [stop #f])
    (if (and (boolean? tid)
             (boolean? cid)
             (boolean? start)
             (boolean? stop))
        #f
        (let* ((data (select db (generate-query SELECT_TASK tid cid start stop))))
          (if (empty? data)
              #f
              (let ((td (second data)))
                (task (vector-ref td 0)
                      (vector-ref td 1)
                      (vector-ref td 2)
                      (vector-ref td 3)
                      (vector-ref td 4)))))))
  
  ; Returns the tasks found with the given parameters.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> or task? boolean?
  (define (select-tasks db
                        #:cid [cid #f]
                        #:start [start #f]
                        #:stop [stop #f])
    (if (and (boolean? cid)
             (boolean? start)
             (boolean? stop))
        #f
        (let* ((data (select db (generate-query SELECT_TASK #f cid start stop))))
          (if (empty? data)
              '()
              (map (lambda (row)
                  (task (vector-ref row 0)
                        (vector-ref row 1)
                        (vector-ref row 2)
                        (vector-ref row 3)
                        (vector-ref row 4)))
              (rest data))))))
  
  (define SELECT_TASKS "select distinct task_id, category_id, name, start, stop from tasks natural join categories where start>=? and stop<=?")
  
  ; Returns the tasks associated with the given category id
  ; and that lie between the start and stop times given.
  ;
  ; db? exact-integer? exact-integer? exact-integer? -> listof task?
  (define (select-tasks-range db start stop #:cid [cid #f])
    (let* ((query (if (boolean? cid)
                      (string-append SELECT_TASKS ";")
                      (string-append SELECT_TASKS " and category_id='" (number->string cid) "';")))
           (data (select db query start stop)))
      (if (empty? data)
          '()
          (map (lambda (row)
                 (task (vector-ref row 0)
                       (vector-ref row 1)
                       (vector-ref row 2)
                       (vector-ref row 3)
                       (vector-ref row 4)))
               (rest data)))))
  
  ; Generates an sql query string by appending the given values when
  ; they are not boolean's to the given query string.  The values
  ; are separated by and's when necessary and completed with a ';'.
  ;
  ; string? string? string? string? string? -> string?
  (define (generate-query query
                          tid
                          cid
                          start
                          stop)
    (let* ((tid-str (if
                     (boolean? tid)
                     #f
                     (string-append "task_id='" (number->string tid) "'")))
           (cid-str (if
                     (boolean? cid)
                     #f
                     (string-append "category_id='" (number->string cid) "'")))
           (start-str (if
                       (boolean? start)
                       #f
                       (string-append "start='" (number->string start) "'")))
           (stop-str (if
                      (boolean? stop)
                      #f
                      (string-append "stop='" (number->string stop) "'")))
           (str (foldl (lambda (str result)
                         (if (boolean? str)
                             result
                             (let ((last (- (string-length result) 1)))
                               (if (and (> last 0)
                                        (string=? (substring result last)
                                                  "'"))
                                   (string-append result " and " str)
                                   (string-append result str)))))
                       query
                       (list tid-str cid-str start-str stop-str))))
      (string-append str ";")))
  )