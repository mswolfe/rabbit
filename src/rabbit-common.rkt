(module rabbit-common racket
  
  ; Requires
  (require (prefix-in time: srfi/19))
  
  ; Provides
  (provide (struct-out task)
           task-total
           
           (struct-out category)
           (struct-out total)
           
           time->date/string
           date/string->time
           
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
  (define (time->date/string ms [now (time:current-date)])
    (if (= ms 0)
        "       "
        (let ((d (time:time-utc->date (time:make-time 'time-utc 0 (round (/ ms 1000))))))
          (if (and
               (= (time:date-year d) (time:date-year now))
               (= (time:date-year-day d) (time:date-year-day now)))
            ; Printing a time for today.
            (time:date->string d "~H:~M:~S")
            ; Past day, print the whole thing.
            (time:date->string d "~m/~d/~y ~H:~M:~S")))))
  
  ; Returns a date object representing the given date string.
  ; Either time in ~H:~M:~S is accepted or time in 
  ; ISO-8601 year-month-day-hour-minute-second format.
  ;
  ; string? -> (or date? boolean?)
  (define (date/string->time str)
    (let ((ms (with-handlers ([(lambda (e) (exn:fail? e))
                     (lambda (e) #f)])
                (let ((d (time:string->date str "~m/~d/~y ~H:~M:~S")))
                  (if (time:date? d)
                      (time->ms (time:date->time-utc d))
                      #f)))))
      (if (boolean? ms)
          (with-handlers ([(lambda (e) (exn:fail? e))
                           (lambda (e) #f)])
            (let ((d (time:string->date (string-append (date-string) " " str) "~m/~d/~y ~H:~M:~S")))
              (if (time:date? d)
                  (time->ms (time:date->time-utc d))
                  #f)))
          ms)))
  
  ; Returns the current date in a string format including the month/day/year.
  ;
  ; void? -> string?
  (define (date-string)
    (time:date->string (time:current-date) "~m/~d/~y"))
  
  ; Returns the given time in milliseconds.
  ;
  ; time? -> exact-integer?
  (define (time->ms t)
    (+
     (* (time:time-second t) 1000)
     (/ (time:time-nanosecond t) 1000000)))
  
  ; Returns a string formatted date given the time in milliseconds.
  ;
  ; exact-integer? -> string?
  (define (time->hours/string ms)
    (let ((str (real->decimal-string (exact->inexact (/ ms 1000 60 60)))))
      (string-append
        str
        " hrs")))
  
  )