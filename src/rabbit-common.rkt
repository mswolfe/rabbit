(module rabbit-common racket
  
  ; Provides
  (provide (struct-out task)
           task-total)
  
  ; The task structure.
  (struct task (tid cid name start stop)
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
  
  )