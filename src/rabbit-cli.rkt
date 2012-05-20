#lang racket
(require racket/date
  
         "rabbit-common.rkt"
         "rabbit.rkt"
         "rabbit-db.rkt")

; TODO: Make this path configurable.
(define db-path (make-parameter #f))

; Command line options.
(define add-c (make-parameter #f))
(define remove-c (make-parameter #f))
(define change-c (make-parameter #f))
(define list-c (make-parameter #f))

(define begin-t (make-parameter #f))
(define end-t (make-parameter #f))
(define end-ts (make-parameter #f))
(define switch-t (make-parameter #f))
(define remove-t (make-parameter #f))
(define list-ttotals (make-parameter #f))

(define list-ctotals (make-parameter #f))
 
; Parse the command line.
(command-line
 #:program "rabbit"
 #:once-each
 [("--database") filename "Sets the database path that rabbit uses."
                 (db-path filename)]
 #:once-any
 [("--add-category") category
                     "Creates a category"
                     (add-c category)]
 [("--remove-category") category
                        "Removes a category"
                        (remove-c category)]
 [("--change-category") old-category new-category
                        "Changes the category name"
                        (remove-c (list old-category new-category))]
 [("--list-category") "Lists all categories"
                        (list-c #t)]
 
 [("-b" "--begin") category "Begin a task for the given category."
                   (begin-t category)]
 [("-e" "--end") "End all currently active tasks."
                 (end-ts #t)]
 [("--end-task") task-id "End the task for the given task id."
                 (end-t (string->number task-id))]
 [("-x" "--switch") category "Switch the active task to the given category.  This will stop all active tasks."
                    (switch-t category)]
 [("-r" "--remove") task-id "Removes the task identified by the given task id."
                    (remove-t (string->number task-id))]
 [("-l" "--list") range "List the tasks for a given range (day/week/month)"
                  (list-ttotals (case range
                                [("week") week]
                                [("month") month]
                                [else day]))]
                  
 [("-o" "--overview") range "List the totals for a given time range (day/week/month)"
                      (list-ctotals (case range
                                      [("week") week]
                                      [("month") month]
                                      [else day]))]
 )

(unless (path-string? (db-path))
  (printf "You must provide the database path with --database <filename>~n")
  (exit 1))

; Past the command line parser, let's setup the db.
(define db (open-db (db-path)))

; Setup the date display formatting.
(date-display-format 'iso-8601)

; Returns a string with the given number of bars.
;
; exact-integer? -> string?
(define (print-bars num-bars [result ""])
  (if (<= num-bars 0)
      result
      (print-bars (sub1 num-bars) (string-append result "="))))

; Add category
(when (string? (add-c))
  (if (add-category db (add-c))
      (printf "Category '~s' created.~n" (add-c))
      (printf "Failed to create category '~s'.~n" (add-c))))

; Remove category
(when (string? (remove-c))
  (if (remove-category db (remove-c))
      (printf "Category '~s' removed.~n" (remove-c))
      (printf "Failed to remove category '~s'.  Is there a task associated with this category?~n" (remove-c))))

; Change category
(when (list? (change-c))
  (if (change-category db
                       (first (change-c))
                       (second (change-c)))
      (printf "Category '~s' change to '~s'.~n" (first (change-c)) (second (change-c)))
      (printf "Failed to change category '~s' to '~s'.~n" (first (change-c)) (second (change-c)))))

; List category
(when (equal? (list-c) #t)
  (foldl (lambda (c)
          (printf "~s~n" (category-name c)))
   (list-categories db)))

; Begin task
(when (string? (begin-t))
  (if (> (begin-task db (begin-t)) 0)
      (printf "Task started for '~s'.~n" (begin-t))
      (printf "Failed to start task for '~s'.~n" (begin-t))))
  
; End tasks
(when (equal? (end-ts) #t)
  (if (end-tasks db)
      (printf "Ended all active tasks.~n")
      (printf "Failed to end active tasks.~n")))
  
; End a specific task.
(when (number? (end-t))
  (if (end-task db (end-t))
      (printf "Ended task ~a.~n" (end-t))
      (printf "Failed to end task ~a.~n" (end-t))))
 
; Switch to a new task.
(when (string? (switch-t))
  (if (> (switch-task db (switch-t)) 0)
    (printf "Task switched to '~s'.~n" (switch-t))
    (printf "Failed to switch to task '~s'.~n" (switch-t))))
    
; Remove a task.
(when (number? (remove-t))
  (if (remove-task db (remove-t))
    (printf "Task '~s' removed.~n" (remove-t))
    (printf "Failed to remove task '~s'.~n" (remove-t))))

; List all tasks.
(when (procedure? (list-ttotals))
  (let* ((tasks (list-tasks db (list-ttotals) (current-milliseconds)))
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
            (time->hours/string total-time))))

; List totals for all categories.
(when (procedure? (list-ctotals))
  (let* ((tasks (list-tasks db (list-ctotals) (current-milliseconds)))
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
            (time->hours/string total))))

; All done, close the database.
(close-db db)
    
