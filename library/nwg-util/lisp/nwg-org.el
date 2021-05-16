(defun nwg/open-message-link (message-id)
  (browse-url-default-macosx-browser
   (concat
    "message:"
    (org-link-encode message-id '(?\< ?\>)))))

(defun nwg/install-custom-org-links ()
  (org-add-link-type "message" 'open-message-link))

(defun nwg/install-org-capture-exit-option ()
  ;; Handling for :exit property
  (defun nwg/capture-finalize ()
    (let* ((template (assoc-default nwg/last-capture-quit nwg-exit-templates-alist)))
      (when (and (not org-note-abort) template)
        (with-current-buffer (marker-buffer org-capture-last-stored-marker)
          (save-excursion
            (goto-char (marker-position org-capture-last-stored-marker))

            (let ((org-capture-templates (list (append (list "*" "Automatic") template))))
              (org-capture nil "*")))))))

  (defun nwg/capture-before-finalize ()
    (setq nwg/last-capture-quit (org-capture-get :exit t)))

  (add-hook 'org-capture-before-finalize-hook #'nwg/capture-before-finalize)
  (add-hook 'org-capture-after-finalize-hook #'nwg/capture-finalize))


(provide 'nwg-org)
