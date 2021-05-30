(defun nwg/newline-and-indent-relative ()
  (interactive)
  (newline)
  (delete-horizontal-space)
  (indent-relative-first-indent-point))

(defun nwg/move-buffer-to-previous-frame ()
  "Move a newly opened buffer to the most-recently-used buffer"
  (interactive)
  (let* ((sw (selected-window))
         (pw (get-mru-window nil nil t))
         (buf (current-buffer)))
    (switch-to-prev-buffer sw nil)
    (set-window-buffer pw buf t)
    (select-window pw nil)))

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

(defun nwg/insert-image (fn &optional properties)
  (let* ((type (and (image-type-available-p 'imagemagick) 'imagemagick))
         (spec (create-image fn type)))
    (let ((begin (point)))
      (insert-image spec)
      (let ((end (point)))
        (when properties
          (add-text-properties begin end properties))))))

(provide 'nwg-ui)
