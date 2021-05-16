
(defun nwg/safe-debugger (&rest debugger-args)
  "Debugger override to just print a stack trace instead"
  (let ((label "nwg/safe-debugger stack")
        (bt (nwg/get-backtrace)))
    (nwg/banner label bt 'external-debugging-output)
    (nwg/banner label bt t)))

(defun nwg/run-safe (c)
  "Run continuation c, capturing all stack traces and printing to"
  "stderr and *messages*"
  (let ((debug-on-error t)
        (debugger #'nwg/safe-debugger))
    (condition-case e
        (progn
          (funcall c)
          'success)
      ((debug error) 'fail))))

(defun nwg/command-error-function (data context caller)
  (message "Got uncaught error %s %s %s" data context caller)
  (let ((standard-output 'external-debugging-output))
    (print (format "Uncaught error: %s %s %s" data context caller)))
  (command-error-default-function data context caller))


(provide 'nwg-debug)
