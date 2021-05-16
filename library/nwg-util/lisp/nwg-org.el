(defun nwg/open-message-link (message-id)
  (browse-url-default-macosx-browser
   (concat
    "message:"
    (org-link-encode message-id '(?\< ?\>)))))

(defun nwg/install-custom-org-links ()
  (org-add-link-type "message" 'open-message-link))

(provide 'nwg-org)
