(module rabbit-common racket
  
  ; Requires
  (require racket/date
           racket/match)
  
  ; Provides
  (provide (struct-out task)
           task-total
           
           (struct-out category)
           (struct-out total)
           
           time->date/string
           time->hours/string)
  
  ; The task structure.
  (struct task (tid cid name start stop)
    #:transparent)
  
  ; The category structure.
  (struct category (cid name)
    #:transparent)
  
  ; The total structure.
  (struct total (cid name time)
    #:transparent)
  
  ; Returns the total amount of time that the task
  ; encompases.  When the stop time is set to zero
  ; the current time will be used to determine
  ; total time.
  ; 
  ; task? -> exact-integer?
  (define (task-total t)
    (let ((stop (task-stop t)))
      (if (= stop 0)
          (- (current-milliseconds) (task-start t))
          (- stop (task-start t)))))
  
  ; Returns a string formatted date given the time in milliseconds.
  ;
  ; exact-integer? -> string?
  (define (time->date/string ms [now (current-date)])
    (if (= ms 0)
        "       "
        (let* ((d (seconds->date (/ ms 1000)))
               (str (date->string d #t)))
          (if (and
               (= (date-year d) (date-year now))
               (= (date-year-day d) (date-year-day now)))
            ; Printing a time for today.
            (string-trim (substring str 11))
            ; Past day, print the whole thing.
            str))))
  
  ; Returns a string formatted date given the time in milliseconds.
  ;
  ; exact-integer? -> string?
  (define (time->hours/string ms)
    (let ((str (real->decimal-string (exact->inexact (/ ms 1000 60 60)))))
      (string-append
        str
        " hrs")))
  )