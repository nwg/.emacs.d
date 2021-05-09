;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(let ((my-path (expand-file-name "/Library/TeX/texbin:/opt/local/bin")))
  (setenv "PATH" (concat my-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-path))

(global-auto-revert-mode t)
(global-display-line-numbers-mode)

;; straight.el setup
(setq straight-use-package-by-default t)
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
(setq straight-use-package-by-default t)

(use-package solarized-theme :straight t)
(load-theme 'solarized-light t)

(setq-default tab-always-indent nil)
(setq-default indent-line-function 'indent-relative-first-indent-point)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
      (setq dired-omit-files
      (concat dired-omit-files
        "\\|" "^.+\\~$"
        ))
            ))
(add-hook 'dired-mode-hook
          (lambda ()
      (dired-omit-mode 1)))
            

(use-package racket-mode
             :straight t
             :ensure t)

(add-hook 'racket-mode 'show-paren-mode)


(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (delete-horizontal-space)
  (indent-relative-first-indent-point))

(defun untab ()
  (interactive)
  (indent-rigidly (- tab-width)))

(defun move-buffer-to-previous-frame (ARG)
  "Move a newly opened buffer to the most-recently-used buffer"
  (interactive "P")
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

(use-package tex
  :straight auctex
  :ensure t
  :defer t
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-auto-cache-preamble t)
  (setq-default TeX-master nil)
  (setq-default TeX-newline-function 'newline-and-indent-relative)
  
  )

(with-eval-after-load 'dired-x
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
    )))

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
  :config
  (setq org-journal-dir "~/Documents/Notes/journal/"
        org-journal-date-format "%A, %d %B %Y")

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  (setq
   org-capture-templates
   (append org-capture-templates
           '(("e" "Email Journal Entry" plain (function org-journal-find-location)
              "** %(format-time-string org-journal-time-format)%^{Title}t\n    %?%i\n    %a")
             ("j" "Journal Entry" plain (function org-journal-find-location)
              "** %(format-time-string org-journal-time-format)%^{Title}t\n    %i%?"
              :jump-to-captured t :immediate-finish t))))

  )
  
(use-package all-the-icons
  :straight t)

(use-package dashboard
  :straight t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Nate's Emacs")
  (setq dashboard-set-navigator t)
  ;; (setq dashboard-show-shortcuts nil)
  (dashboard-modify-heading-icons '((recents . "file-text")))
  
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner "~/.emacs.d/tree.png")
  (setq dashboard-center-content t)
  (global-set-key (kbd "C-M-s-SPC") (lambda () (interactive) (switch-to-buffer "*dashboard*")))

  (dashboard-setup-startup-hook))

(server-start)
