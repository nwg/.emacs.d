
(defun nwg/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (let ((w (active-minibuffer-window)))
    (if w
        (select-window w)
      (error "Minibuffer is not active"))))

(defun nwg/current-project-name ()
  "Projectile project name or '-'"
  (or
   (and (minorp projectile) (projectile-project-name))
   "-"))

(defun nwg/project-file-name ()
  "Filename with path, but only so far to root as projectile project base"
  (and buffer-file-name
       (minorp projectile)
       (file-relative-name buffer-file-name (projectile-project-root))))

(defun nwg/open-current-buffer-in-selection ()
  (interactive)
  "Opens current buffer in a window under current mouse position"
  (let ((buf (window-buffer))
        (window (window-at (cadr (mouse-position))
                           (cddr (mouse-position))
                           (car (mouse-position)))))
    (with-selected-window window
      (switch-to-buffer buf))))

(provide 'nwg-ui)
