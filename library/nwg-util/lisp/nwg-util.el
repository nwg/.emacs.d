(require 'nwg-debug)
(require 'nwg-ui)
(require 'nwg-org)
(require 'nwg-curry-compose)

; Call setter function if available; don't use the customization automatic edit system
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

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

(defun nwg/reset-path-to-user-profile ()
  (let* ((path-from-profile (shell-command-to-string ". $HOME/.profile; echo $PATH")))
    (setenv "PATH" path-from-profile)))

(defun nwg/prepend-if-missing (candidates list)
  (append
   (cl-remove-if (rcurry #'member list) candidates)
   list))

(defun nwg/add-to-path-env (candidates)
  (let* ((path (split-string (getenv "PATH") ":"))
         (my-path (nwg/prepend-if-missing candidates path)))
    (setenv "PATH" (s-join ":" my-path))))

(defun nwg/add-to-exec-path (candidates)
  (let* ((my-exec-path (nwg/prepend-if-missing candidates (symbol-value 'exec-path))))
    (setq exec-path my-exec-path)))

(provide 'nwg-util)
