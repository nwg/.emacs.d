(require 'nwg-debug)

(defmacro minorp (sym)
  (let* ((sym-mode-s (concat (symbol-name sym) "-mode"))
         (sym-mode (intern sym-mode-s)))
    `(and (boundp ',sym-mode) ',sym-mode)))

(defun nwg/get-backtrace ()
  "Get backtrace to string and return it"
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace)
      (buffer-string))))

(defun nwg/banner (label content &optional pcf)
  "Show a banner for `label` with `content`. Use pcf for object sent along to print"
  (print (format "------------ begin %s ------------" label) pcf)
  (print content pcf)
  (print (format "------------ end %s ------------" label) pcf))

(defun nwg/dump-trace (label)
  "Dump current trace to echo area"
  (let ((bt (nwg/get-backtrace)))
    (nwg/banner label bt t)0))

(provide 'nwg-util)
