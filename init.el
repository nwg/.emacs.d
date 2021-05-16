;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq debug-on-error t)

(require 'cl-lib)

(let ((my-path (expand-file-name "/Library/TeX/texbin:/opt/local/bin")))
  (setenv "PATH" (concat my-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-path))

;; --- begin straight.el setup
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
;; --- end straight.el setup

; Some support packages needed by package setup
(use-package f
  :straight t)

; Call setter function if available; don't use the customization automatic edit system
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

; Mac OS X Cmd-click maps to middle mouse
(when (eq system-type 'darwin)
  (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>")))

; Highlight whitespace
(setq-default whitespace-style '(face trailing tabs spaces))
(global-auto-revert-mode t)
(global-display-line-numbers-mode)
(global-whitespace-mode 1)

; Indentation
(setq-default tab-always-indent nil)
(setq-default indent-line-function 'indent-relative-first-indent-point)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; Globally available dirs
(setq documents-dir "~/Documents")
(setq notes-dir (f-join documents-dir "Notes"))
(setq journal-dir (f-join notes-dir "Journal"))
(setq journal-file (f-join journal-dir "Journal.org"))
(setq unfiled-dir (f-join notes-dir "Unfiled"))
(setq unfiled-file (f-join unfiled-dir "Unfiled.org"))
(setq org-dir (f-join documents-dir "Org"))
                                        ;(require 'cons)
;; (setq
;;  'display-buffer-alist
;;  (append
;;   display-buffer-alist
;;   '(("^\\*Help\\*$" .
;;      ((display-buffer-reuse-window)
;;        ;; display-buffer-pop-up-window)
;;       (inhibit-same-window . t))))))


;; Show project in title
;; Note -- this is the wrong way to do this
;; I am just demonstrating debugging
(defun nwg/get-backtrace ()
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace)
      (buffer-string))))

(defun nwg/banner (label content &optional pcf)
  (print (format "------------ begin %s ------------" label) pcf)
  (print content pcf)
  (print (format "------------ end %s ------------" label) pcf))

(defun nwg/safe-debugger (&rest debugger-args)
  (let ((label "nwg/safe-debugger stack")
        (bt (nwg/get-backtrace)))
    (nwg/banner label bt 'external-debugging-output)
    (nwg/banner label bt t)))

(defun nwg/run-safe (c)
  (let ((debug-on-error t)
        (debugger #'nwg/safe-debugger))
    (condition-case e
        (progn
          (funcall c)
          'success)
      ((debug error) 'fail))))

(defmacro minorp (sym)
  (let* ((sym-mode-s (concat (symbol-name sym) "-mode"))
         (sym-mode (intern sym-mode-s)))
    `(and (boundp ',sym-mode) ',sym-mode)))

(defun mytest ()
  (nwg/run-safe (lambda () (error "hi"))))

(defun nwg/current-project-name ()
  (or
   (and (minorp projectile) (projectile-project-name))
   "-"))

(defun nwg/project-file-name ()
  (and buffer-file-name
       (minorp projectile)
       (file-relative-name buffer-file-name (projectile-project-root))))

(defun nwg/command-error-function (data context caller)
  (message "Got uncaught error %s %s %s" data context caller)
  (let ((standard-output 'external-debugging-output))
    (print (format "Uncaught error: %s %s %s" data context caller)))
  (command-error-default-function data context caller))

(setq command-error-function #'nwg/command-error-function)

(defun nwg/setup-title ()
  (let* ((fn (or (nwg/project-file-name) "%b")))
    (setq frame-title-format (format "%s :: %s" (nwg/current-project-name) fn))))

(defun nwg/buffer-list-update ()
  (nwg/run-safe #'nwg/setup-title))

(defun nwg/window-configuration-change ()
  (nwg/run-safe #'nwg/setup-title))

(add-hook 'buffer-list-update-hook #'nwg/buffer-list-update 'append)
(add-hook 'window-configuration-change-hook #'nwg/window-configuration-change'append)

; Opens current buffer in a window under current mouse position
(defun open-current-buffer-in-selection ()
  (lambda ()
    (interactive)

    (let ((buf (window-buffer))
          (window (window-at (cadr (mouse-position))
                              (cddr (mouse-position))
                              (car (mouse-position)))))
      (with-selected-window window
        (switch-to-buffer buf)))))

(global-set-key (kbd "C-x w <mouse-1>") (open-current-buffer-in-selection))

; Quick Keys for some common files
(global-set-key (kbd "C-h H") (lambda () (interactive) (switch-to-buffer "*Help*")))
(global-set-key (kbd "C-M-s-<return>") (lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "M-s-<return>") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c C-j") (lambda () (interactive) (find-file journal-file)))

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

; Recent file keys
(cl-loop for i from 0 to 9
      do (let ((keys (format "C-x r %d" i))
               (sym (format "recentf-open-most-recent-file-%d" i)))
           (global-set-key (kbd keys) (intern sym))))

(use-package ivy
  :straight t
  :config
  (ivy-mode 1)

  (defun nwg/ivy-read-custom-display (key prefix candidates &rest rest)
    (let* ((custom-candidates (mapcar key candidates))
           (display-map (cl-mapcar #'cons custom-candidates candidates))
           (i (list prefix custom-candidates))
           (orig-action (plist-get rest ':action))
      )
      (defun action (c)
        (funcall orig-action (cdr (assoc c display-map))))

      (apply #'ivy-read (append i (plist-put rest ':action #'action)))
      ))

  )

(use-package counsel
  :straight t)

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

(setq notes-files
      (directory-files-recursively notes-dir "\\.org$"))

(defun nwg/find-note ()
  (interactive)
  (nwg/ivy-read-custom-display
   #'file-name-nondirectory
   "Note: "
   notes-files
   :action #'find-file :caller 'nwg/find-note))

(use-package org
  :straight t
  :custom
  (org-adapt-indentation t)
  (org-hide-leading-stars t)
  (org-id-link-to-org-use-id t)
  (org-complete-tags-always-offer-all-agenda-tags t)
  (org-agenda-include-diary t)
  (org-log-done 'time)
  (org-clock-in-resume t "Just resume open clock on explicit clock in (normally does resolve)")
  (org-agenda-window-setup 'current-window)
  (org-agenda-files notes-files)
  (org-clock-persist t)
  (org-cycle-emulate-tab 'white)
  (org-support-shift-select t)
  (org-directory org-dir)
  (org-default-notes-file unfiled-file)
  (org-indent-indentation-per-level 2)
  (org-refile-targets '((nil :maxlevel . 9) ; nil means local
                        (org-agenda-files :maxlevel . 9))
                      "Local and all agenda headings up to level 9")

  (org-outline-path-complete-in-steps nil "Ivy seems broken when steps on")
  (org-refile-use-outline-path 'file "Show file and full node paths for refiling")
  (org-refile-allow-creating-parent-nodes 'confirm "Autocreate parents with confirmation")
  :config

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c C-x g") 'org-agenda-clock-goto)
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)

  (org-clock-persistence-insinuate)

  (csetq org-bookmark-names-plist
         (quote (:last-capture "Last Org Capture"
                 :last-refile "Last Org Refile"
                 :last-capture-marker "Last Org Capture Marker")))

  (csetq org-capture-templates
         (quote (("t" "todo" entry (file org-default-notes-file)
                  "\n* TODO %?\n  %U\n  Context: %a\n" :clock-in t :clock-resume t :exit "log-todo-to-journal")
                 ("r" "respond" entry (file default-notes-file)
                  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                 ("n" "note" entry (file org-default-notes-file)
                  "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                 ("j" "Journal" entry (file+olp+datetree journal-file)
                  "* %?\n%(nwg/I)Context: %a\n%(nwg/I)%U\n" :clock-in t :clock-resume t)
                 ("w" "org-protocol" entry (file org-default-notes-file)
                  "* TODO Review %c\n%U\n" :immediate-finish t)
                 ("m" "Meeting" entry (file org-default-notes-file)
                  "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                 ("p" "Phone call" entry (file org-default-notes-file)
                  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                 ("h" "Habit" entry (file org-default-notes-file)
                  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  (defun nwg/org-agenda-mode-setup ()
    ;; Always hilight the current agenda line
    (hl-line-mode 1))

  (add-hook 'org-agenda-mode-hook #'nwg/org-agenda-mode-setup 'append)

  (defun open-message-link (message-id)
    (browse-url-default-macosx-browser
     (concat
      "message:"
      (org-link-encode message-id '(?\< ?\>)))))

  (org-add-link-type "message" 'open-message-link)

  (defun nwg/I ()
    "  ")

  (defun nwg/fmt-clk (clock-link)
    (if org-clock-current-task
        (format "%sCurrent Task: %s\n" (nwg/I) clock-link)
      ""))

  ;; Set up post-capture actions in nwg-exit-templates-alist
  ;; car of each item be pointed to by custom `:exit` property in normal
  ;; org-capture-templates
  (let ((post-todo '(entry (file+olp+datetree journal-file)
                           "* Created new TODO: %a\n%(nwg/fmt-clk \"%K\")%(nwg/I)%U\n"
                           :immediate-finish t)))

    (setq nwg-exit-templates-alist
          `(("log-todo-to-journal" . ,post-todo))))

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
  (add-hook 'org-capture-after-finalize-hook #'nwg/capture-finalize)


  )

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

  (setq dashboard-bookmarks-item-format "%s")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner "~/.emacs.d/tree.png")
  (setq dashboard-center-content t)
  (global-set-key (kbd "C-M-s-SPC") (lambda () (interactive) (switch-to-buffer "*dashboard*")))

  (setq dashboard-items '((bookmarks . 5)
                          (recents  . 5)
                          (agenda . 5)
                          (projects . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook))

(require 'org-protocol)
(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
