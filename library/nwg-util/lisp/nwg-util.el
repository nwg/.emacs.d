(defmacro nwg-ensure (arg)
  `(if (featurep ,arg)
       (load-library (symbol-name ,arg))
     (require ,arg)))

(nwg-ensure 'nwg-debug)
(nwg-ensure 'nwg-ui)
(nwg-ensure 'nwg-org)
(nwg-ensure 'nwg-curry-compose)

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

(defun nwg/user-profile-path ()
  (shell-command-to-string ". $HOME/.profile; echo $PATH"))

(defun nwg/prepend-if-missing (candidates list)
  (append
   (cl-remove-if (rcurry #'member list) candidates)
   list))

(defun nwg/add-to-path-env (candidates)
  (let* ((expanded-candidates (mapcar #'expand-file-name candidates))
         (path (split-string (getenv "PATH") ":"))
         (my-path (nwg/prepend-if-missing expanded-candidates path)))
    (setenv "PATH" (s-join ":" my-path))))

(defun nwg/add-to-exec-path (candidates)
  (let* ((expanded-candidates (mapcar #'expand-file-name candidates))
         (my-exec-path (nwg/prepend-if-missing expanded-candidates (symbol-value 'exec-path))))
    (setq exec-path my-exec-path)))

(provide 'nwg-util)
