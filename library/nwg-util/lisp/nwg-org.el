(defun nwg/open-message-link (message-id)
  (browse-url-default-macosx-browser
   (concat
    "message:"
    (org-link-encode message-id '(?\< ?\>)))))

(defun nwg/install-custom-org-links ()
  (org-add-link-type "message" 'nwg/open-message-link))

(defun nwg/capture-finalize ()
  (when (not org-note-abort)
    (let* ((template (alist-get nwg/last-capture-quit nwg-exit-templates-alist)))
      (when template
        (with-current-buffer (marker-buffer org-capture-last-stored-marker)
          (save-excursion
            (goto-char (marker-position org-capture-last-stored-marker))

            (let ((org-capture-templates (list (append (list "*" "Automatic") template))))
              (org-capture nil "*"))))))))

(defun nwg/capture-before-finalize ()
  (setq nwg/last-capture-quit (org-capture-get :exit t)))

(defun nwg/install-org-capture-exit-option ()
  ;; Handling for :exit property
  (add-hook 'org-capture-before-finalize-hook #'nwg/capture-before-finalize)
  (add-hook 'org-capture-after-finalize-hook #'nwg/capture-finalize))


(provide 'nwg-org)
