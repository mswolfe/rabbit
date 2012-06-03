#lang racket
(require racket/date
  
         "rabbit-common.rkt"
         "rabbit.rkt"
         "rabbit-db.rkt")

; Command line options.
(define db-path (make-parameter #f))

(define add-c (make-parameter #f))
(define remove-c (make-parameter #f))
(define change-c (make-parameter #f))
(define list-c (make-parameter #f))

(define begin-t (make-parameter #f))
(define end-ts (make-parameter #f))
(define switch-t (make-parameter #f))

(define end-t (make-parameter #f))
(define remove-t (make-parameter #f))
(define update-t-id (make-parameter #f))
(define update-t-start (make-parameter #f))
(define update-t-stop (make-parameter #f))
(define update-t-category (make-parameter #f))

(define list-ttotals (make-parameter #f))
(define list-ctotals (make-parameter #f))

(define version (make-parameter #f))
 
; Parse the command line.
(command-line
 #:program "rabbit"
 #:once-each
 [("--database") filename "Sets the database path that rabbit uses."
                 (db-path filename)]
 #:once-any
 ; Category manipulation flags.
 [("--add-category") category
                     "Creates a category"
                     (add-c category)]
 [("--remove-category") category
                        "Removes a category"
                        (remove-c category)]
 [("--change-category") old-category new-category
                        "Changes the category name"
                        (change-c (list old-category new-category))]
 [("--list-category") "Lists all categories"
                        (list-c #t)]
 
 ; Logical task starting/switching/ending                        
 [("-b" "--begin") category "Begin a task for the given category."
                   (begin-t category)]
 [("-e" "--end") "End all currently active tasks."
                 (end-ts #t)]
 [("-x" "--switch") category "Switch the active task to the given category.  This will stop all active tasks."
                    (switch-t category)]
 
 ; Listing tasks and overviews.
 [("-l" "--list") range "List the tasks for a given range where range can be the number of days in integer form or one of the following defined constants: day|week|month|year"
                  (list-ttotals range)]
                  
 [("-o" "--overview") range "List the totals for a given range where range can be the number of days in integer form or one of the following defined constants: day|week|month|year"
                      (list-ctotals range)]
 
 ; Task manipulations.
 [("--end-task") task-id "End the task for the given task id."
                 (end-t task-id)]
 [("--remove-task") task-id "Removes the task identified by the given task id."
                    (remove-t task-id)]
 [("--update-task-start") task-id start "Updates the starting time for the given task id."
                          (begin (update-t-id task-id)
                                 (update-t-start start))]
 [("--update-task-stop") task-id stop "Updates the stop time for the given task id."
                          (begin (update-t-id task-id)
                                 (update-t-stop stop))]
 [("--update-task-category") task-id category "Updates the category for the given task id."
                          (begin (update-t-id task-id)
                                 (update-t-category category))]
 [("-v" "--version") "Prints the version information."
                     (version #t)]
 
 )

; Print the version info and exit when asked.
(when (version)
  (printf "Rabbit ~a~n" +version+)
  (exit 0))

(unless (path-string? (db-path))
  (printf "You must provide the database path with --database <filename>~n")
  (exit 1))

; Past the command line parser, let's setup the db.
(define db (open-db (db-path)))

; Returns a string with the given number of bars.
;
; exact-integer? -> string?
(define (print-bars num-bars [result ""])
  (if (<= num-bars 0)
      result
      (print-bars (sub1 num-bars) (string-append result "="))))

; Returns the procedure or start time to search upon.
;
; string? -> (or procedure? exact-integer?)
(define (find-range str)
  (cond
   [(string=? str "day") day]
   [(string=? str "week") week]
   [(string=? str "month") month]
   [(string=? str "year") year]
   [else
     (with-handlers ([(lambda (e) (exn:fail? e))
                      (lambda (e) day)])
       (let* ((now (current-date))
              (num (string->number str))
              (time (+ 
                      (* (date-hour now) 60 60 1000)
                      (* (date-minute now) 60 1000)
                      (* (date-second now) 1000)
                      (* (- num 1) 24 60 60 1000))))
         (- (current-milliseconds) time)))]))

(cond
  ; Add category
  [(string? (add-c))
   (if (add-category db (add-c))
       (printf "Category '~a' created.~n" (add-c))
       (printf "Failed to create category '~a'.~n" (add-c)))]
   
  ; Remove category
  [(string? (remove-c))
    (if (remove-category db (remove-c))
        (printf "Category '~a' removed.~n" (remove-c))
        (printf "Failed to remove category '~a'.  Is there a task associated with this category?~n" (remove-c)))]
    
  ; Change category
  [(list? (change-c))
    (if (change-category db
                         (first (change-c))
                         (second (change-c)))
        (printf "Category '~a' change to '~a'.~n" (first (change-c)) (second (change-c)))
        (printf "Failed to change category '~a' to '~a'.~n" (first (change-c)) (second (change-c))))]

  ; List category
  [(equal? (list-c) #t)
    (for-each (lambda (c)
                (printf "~a~n" (category-name c)))
     (list-categories db))]

  ; Begin task
  [(string? (begin-t))
    (if (> (begin-task db (begin-t)) 0)
        (printf "Task started for '~a'.~n" (begin-t))
        (printf "Failed to start task for '~a'.~n" (begin-t)))]
  
  ; End tasks
  [(equal? (end-ts) #t)
    (if (end-tasks db)
        (printf "Ended all active tasks.~n")
        (printf "Failed to end active tasks.~n"))]
 
  ; Switch to a new task.
  [(string? (switch-t))
    (if (> (switch-task db (switch-t)) 0)
      (printf "Task switched to '~a'.~n" (switch-t))
      (printf "Failed to switch to task '~a'.~n" (switch-t)))]
    
  ; End a specific task.
  [(string? (end-t))
    (if (end-task db (string->number (end-t)))
        (printf "Ended task ~a.~n" (end-t))
        (printf "Failed to end task ~a.~n" (end-t)))]
    
  ; Remove a task.
  [(string? (remove-t))
    (if (remove-task db (string->number (remove-t)))
      (printf "Task '~a' removed.~n" (remove-t))
      (printf "Failed to remove task '~a'.~n" (remove-t)))]
    
  ; Update a task start time.
  [(and (string? (update-t-id))
        (string? (update-t-start)))
   (if (update-task db
                    (string->number (update-t-id))
                    #:start (date/string->time (update-t-start)))
       (printf "Updated start time for task id '~a'.~n" (update-t-id))
       (printf "Failed to update start time for task id '~a'.~n" (update-t-id)))]
                    
  ; Update a task stop time.
  [(and (string? (update-t-id))
        (string? (update-t-stop)))
   (if (update-task db
                    (string->number (update-t-id))
                    #:stop (date/string->time (update-t-stop)))
       (printf "Updated stop time for task id '~a'.~n" (update-t-id))
       (printf "Failed to update stop time for task id '~a'.~n" (update-t-id)))]
      
  ; Update a task category.
  [(and (string? (update-t-id))
        (string? (update-t-category)))
   (let ((cid (select-category-id db (update-t-category))))
     (if (= cid -1)
         (printf "Failed to find the given category!  Update failed.")
         (if (update-task db
                          (string->number (update-t-id))
                          #:cid cid)
             (printf "Updated category for task id '~a'.~n" (update-t-id))
             (printf "Failed to category for task id '~a'.~n" (update-t-id)))))]

  ; List all tasks.
  [(string? (list-ttotals))
    (let* ((range (find-range (list-ttotals)))
           (tasks (list-tasks db range (current-milliseconds)))
           (total-time (foldl (lambda (t result)
                                (+ result (task-total t)))
                               0
                               tasks)))
      (for-each (lambda (t)
                 (printf "[~a] ~a (~a - ~a) ~a~n"
                         (number->string (task-tid t))
                         (task-name t)
                         (time->date/string (task-start t))
                         (time->date/string (task-stop t))
                         (time->hours/string (task-total t))))
        tasks)
      (printf "Total: ~a~n"
              (time->hours/string total-time)))]

  ; List totals for all categories.
  [(string? (list-ctotals))
    (let* ((range (find-range (list-ctotals)))
           (tasks (list-tasks db range (current-milliseconds)))
           (total (foldl (lambda (t result)
                            (+ result (task-total t)))
                           0
                           tasks)))
      (for-each (lambda (t)
                   (let* ((name (total-name t))
                          (time (total-time t)) 
                          ; Normalize to 10 bars.
                          (numbars (round (* (/ time total) 10))))
                     (printf "~a ~a | ~a>~n"
                             name
                             (time->hours/string time)
                             (print-bars numbars))))
        (list-category-totals tasks))
      (printf "Total: ~a~n"
              (time->hours/string total)))])

; All done, close the database.
(close-db db)
    
