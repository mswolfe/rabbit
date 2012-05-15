#lang racket
(require "rabbit.rkt"
         "rabbit-db.rkt")

(define DB_PATH "/home/mwolfe/Documents/Dropbox/work.rabbit")
;(define DB_PATH "rabbit.db")

(define start (make-parameter #f))
(define stop (make-parameter #f))
(define change-category (make-parameter #f))
(define tasks (make-parameter #f))
(define totals (make-parameter #f))
 
(command-line
 #:program "rabbit"
 #:once-any
 [("-s" "--start") category "Start a task for the given category."
                   (start category)]
 [("-x" "--stop") category "Stop the current task for the given category."
                  (stop category)]
 [("-c" "--category") task-id category "Switch the category for the given task id."
                      (change-category (list task-id category))]
 [("-t" "--tasks") range "List the tasks for a given time range (day/week/month)"
                  (tasks (case range
                           [("week") 'week]
                           [("month") 'month]
                           [else 'day]))]
  [("-o" "--totals") range "List the totals for a given time range (day/week/month)"
                  (totals (case range
                            [("week") (totals 'week)]
                            [("month") (totals 'month)]
                            [else 'day]))]
  )

(when (and (not (boolean? (start)))
           (string? (start)))
  (let* ((db (open-db DB_PATH))
         (category (start))
         (id (start-task db category)))
    (printf "Task ~a started for ~s.~n" id category)
    (close-db db)))

(when (and (not (boolean? (stop)))
           (string? (stop)))
  (let* ((db (open-db DB_PATH))
         (category (stop))
         (success (stop-task db category)))
    (if success
        (printf "Task stopped for ~s.~n" category)
        (printf "Failed to stop task for ~s.~n" category))
    (close-db db)))

(unless (boolean? (tasks))
  (let* ((db (open-db DB_PATH))
         (data (case (tasks)
                 [('week) (get-tasks/week db)]
                 [('month) (get-tasks/month db)]
                 [else (get-tasks/today db)])))
    (for-each (lambda (t)
                (printf "[~a] ~a: ~a - ~a [~a hrs]~n"
                        (task-task-id t)
                        (task-category-name t)
                        (task-start t)
                        (task-stop t)
                        (if (= (task-stop t) 0)
                            (exact->inexact (/ (- (current-milliseconds) (task-start t))
                                               1000 60 60))
                            (exact->inexact (/ (- (task-stop t) (task-start t))
                                               1000 60 60)))))
              data)
    (close-db db)))

(unless (boolean? (totals))
  (let* ((db (open-db DB_PATH))
         (data (case (tasks)
                 [('week) (get-category-totals/week db)]
                 [('month) (get-category-totals/month db)]
                 [else (get-category-totals/today db)])))
    (for-each (lambda (t)
                (if (total? t)
                    (printf "~a: ~a hrs~n"
                            (total-name t)
                            (exact->inexact (/ (total-time t)
                                               1000 60 60)))
                    (printf "Total: ~a hrs~n"
                            (exact->inexact (/ t 1000 60 60)))))
              data)
    (close-db db)))

(unless (boolean? (change-category))
  (let* ((db (open-db DB_PATH))
         (task-id (first (change-category)))
         (category (second (change-category)))
         (success (update-task-category db task-id category)))
    (if success
        (printf "Changed the category for task ~s to ~s.~n" task-id category)
        (printf "Failed to change the category for task ~s to ~s.~n" task-id category))
    (close-db db)))
    
