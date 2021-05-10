;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(let ((my-path (expand-file-name "/Library/TeX/texbin:/opt/local/bin")))
  (setenv "PATH" (concat my-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-path))

;; straight.el setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; end straight.el setup

(global-auto-revert-mode t)
(global-display-line-numbers-mode)

(setq-default tab-always-indent nil)
(setq-default indent-line-function 'indent-relative-first-indent-point)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq display-buffer-alist
      (append
       display-buffer-alist
       '(("*Help*" display-buffer-reuse-window) 
         ("*Help*" display-buffer-use-some-window))))

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (delete-horizontal-space)
  (indent-relative-first-indent-point))

(defun untab ()
  (interactive)
  (indent-rigidly (- tab-width)))

(defun move-buffer-to-previous-frame ()
  "Move a newly opened buffer to the most-recently-used buffer"
  (interactive)
  (let* ((sw (selected-window))
         (pw (get-mru-window nil nil t))
         (buf (current-buffer)))
    (switch-to-prev-buffer sw nil)
    (set-window-buffer pw buf t)
    (select-window pw nil)))

(global-set-key (kbd "C-x r R") 'recentf-open-files)
(global-set-key (kbd "C-x w o") 'window-swap-states)
(global-set-key (kbd "C-x w -") 'move-buffer-to-previous-frame)

(cl-loop for i from 0 to 9
      do (let ((keys (format "C-x r %d" i))
               (sym (format "recentf-open-most-recent-file-%d" i)))
           (global-set-key (kbd keys) (intern sym))))


(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-light t))

(use-package dired-x
  :after dired
  :config

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|" "\\.pdf$"
                "\\|" "\\.aux$"
                "\\|" "\\.log$"
                "\\|" "\\.prv$"
                "\\|" "^_region_\\.log$"
                "\\|" "^_region_\\.prv"
                "\\|" "^_region_\\.tex$"
                "\\|" "^.DS_Store$"
                "\\|" "^.+\\~$"
                ))
  
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-omit-mode 1)))
  )

(use-package racket-mode
  :straight t
  :config
  (add-hook 'racket-mode 'show-paren-mode)
  )

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-auto-cache-preamble t)
  (setq-default TeX-master nil)
  (setq-default TeX-newline-function 'newline-and-indent-relative)
  
  )

(defun open-message-link (message-id)
  (browse-url-default-macosx-browser
   (concat
    "message:"
    (org-link-encode message-id '(?\< ?\>)))))

(use-package org
  :straight t
  :config
  (require 'org-protocol)

  (org-add-link-type "message" 'open-message-link)

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-cycle-emulate-tab 'white)
  (setq org-support-shift-select t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/Notes/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")))
  (setq org-adapt-indentation t
        org-hide-leading-stars t) 

  (setq org-bookmark-names-plist
        '(last-capture "Last Org Capture"
          last-refile "Last Org Refile"
          last-capture-marker "Last Org Capture Marker"))

  (add-hook
   'org-src-mode-hook
   (lambda ()
     (setq tab-always-indent nil)))
  
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq tab-always-indent t)))

  )

(use-package org-journal
  :straight t
  :bind
  ("C-c j" . org-journal-new-entry)
  ;; ("C-c j d" . org-journal-new-date-entry)
  ;; ("C-c j s" . org-journal-new-scheduled-entry)
  :config
  (setq org-journal-dir "~/Documents/Notes/Journal/"
        org-journal-date-format "%A, %d %B %Y")

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  (setq org-journal-enable-agenda-integration t
        org-icalendar-store-UID t
        org-icalendar-include-todo "all"
        org-icalendar-combined-agenda-file "~/Dropbox/emacs/org-agenda.ics")
  
  (setq
   org-capture-templates
   (append org-capture-templates
           '(("e" "Email Journal Entry" plain (function org-journal-find-location)
              "** %(format-time-string org-journal-time-format)%^{Title}\n    %?%i\n    %a"
              :jump-to-captured t)
             ("j" "Journal Entry" plain (function org-journal-find-location)
              "** %(format-time-string org-journal-time-format)%^{Title}\n    %i%i\n    %a"
              :jump-to-captured t))))

  )
  
(use-package all-the-icons
  :straight t)

(use-package dashboard
  :straight t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Nate's Emacs")
  (setq dashboard-set-navigator t)
  (dashboard-modify-heading-icons '((recents . "file-text")))
  
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner "~/.emacs.d/tree.png")
  (setq dashboard-center-content t)
  (global-set-key (kbd "C-M-s-SPC") (lambda () (interactive) (switch-to-buffer "*dashboard*")))

  (setq dashboard-items '((bookmarks . 5)
                          (recents  . 5)
                          (agenda . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook))

(server-start)
