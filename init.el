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

(add-to-list 'load-path (f-join user-emacs-directory "library/nwg-util/lisp"))
(require 'nwg-util)

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

(setq notes-files
      (directory-files-recursively notes-dir "\\.org$"))

;; (setq
;;  'display-buffer-alist
;;  (append
;;   display-buffer-alist
;;   '(("^\\*Help\\*$" .
;;      ((display-buffer-reuse-window)
;;        ;; display-buffer-pop-up-window)
;;       (inhibit-same-window . t))))))


(setq command-error-function #'nwg/command-error-function)

(setq frame-title-format
      `((:eval (nwg/current-project-name))
        " :: "
        (:eval (or (nwg/project-file-name) (buffer-name)))))

(global-set-key (kbd "C-x w <mouse-1>") #'nwg/open-current-buffer-in-selection)
(global-set-key (kbd "C-x C-<return>") #'nwg/switch-to-minibuffer)
(global-set-key (kbd "C-x C-g") #'minibuffer-keyboard-quit)
(global-set-key (kbd "C-x r R") #'counsel-recentf)

; Quick Keys for some common files
(global-set-key (kbd "C-h H") (lambda () (interactive) (switch-to-buffer "*Help*")))
(global-set-key (kbd "C-M-s-<return>") (lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "M-s-<return>") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c C-j") (lambda () (interactive) (find-file journal-file)))
(global-set-key (kbd "C-x w o") 'window-swap-states)
(global-set-key (kbd "C-x w -") #'nwg/move-buffer-to-previous-frame)

; Recent file keys
(cl-loop for i from 0 to 9
      do (let ((keys (format "C-x r %d" i))
               (sym (format "recentf-open-most-recent-file-%d" i)))
           (global-set-key (kbd keys) (intern sym))))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :custom
  (ivy-prescient-retain-classic-highlighting t "Use the old ivy highlighting style")
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :straight t
  :config
  (company-prescient-mode 1))

(use-package ivy
  :straight t
  :custom
  (ivy-height 20 "Taller ivy")
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
  :straight t
  :bind (("C-x r m" . counsel-bookmark)))

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

  (defun nwg/dired-new-file ()
    (interactive)
    (let* ((fn "New File")
           (buf (generate-new-buffer fn)))
      (switch-to-buffer buf)))

  (defun nwg/dired-new-file-2 ()
    (interactive)
    (let* ((buf (current-buffer)))
      (call-interactively #'find-file)))

  (define-key dired-mode-map (kbd "n") #'nwg/dired-new-file-2)

  (defun nwg/setup-dired ()
    (dired-omit-mode 1))

  (add-hook 'dired-mode-hook #'nwg/setup-dired))

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
  (setq-default TeX-newline-function #'nwg/newline-and-indent-relative)

  )

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
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
  (global-set-key (kbd "C-c C-w") 'org-refile)

  (define-key org-mode-map (kbd "C-c C-q") #'counsel-org-tag)

  (defun nwg/org-jump-to-content ()
    (interactive)
    (deactivate-mark t)
    (let ((start (point)))
      (org-next-visible-heading 1)
      (whitespace-cleanup-region start (point)))
    (open-line 1)
    (org-indent-line))

  (define-key org-mode-map (kbd "C-c o") #'nwg/org-jump-to-content)

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c C-q") #'counsel-org-tag-agenda))

  (org-clock-persistence-insinuate)

  (csetq org-bookmark-names-plist
         (quote (:last-capture "Last Org Capture"
                 :last-refile "Last Org Refile"
                 :last-capture-marker "Last Org Capture Marker")))

  (csetq org-capture-templates
         (quote (("t" "todo" entry (file org-default-notes-file)
                  "\n* TODO %?\n%(II)%U\%(II)Context: %a\n" :clock-in t :clock-resume t :exit "log-todo-to-journal")
                 ("r" "respond" entry (file default-notes-file)
                  "* NEXT Respond to %:from on %:subject\n%(II)SCHEDULED: %t\n%(II)%U\n%(II)%a\n" :clock-in t :clock-resume t :immediate-finish t)
                 ("n" "note" entry (file org-default-notes-file)
                  "* %? :note:\n%(II)%U\n%(II)%a\n" :clock-in t :clock-resume t)
                 ("e" "email" entry (file org-default-notes-file)
                  "* %? :email:\n%(II)%U\n%(II)%a\n" :clock-in t :clock-resume t)
                 ("j" "Journal" entry (file+olp+datetree journal-file)
                  "* %?\n%(II)Context: %a\n%(II)%U\n" :clock-in t :clock-resume t)
                 ("w" "org-protocol" entry (file org-default-notes-file)
                  "* TODO Review %c\n%(II)%U\n" :immediate-finish t)
                 ("m" "Meeting" entry (file org-default-notes-file)
                  "* MEETING with %? :MEETING:\n%(II)%U" :clock-in t :clock-resume t)
                 ("p" "Phone call" entry (file org-default-notes-file)
                  "* PHONE %? :PHONE:\n%(II)%U" :clock-in t :clock-resume t)
                 ("h" "Habit" entry (file org-default-notes-file)
                  "* NEXT %?\n%(II)%U\n%(II)%a\n%(II)SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n%(II):PROPERTIES:\n%(II):STYLE: habit\n%(II):REPEAT_TO_STATE: NEXT\n%(II):END:\n"))))

  (defun nwg/org-agenda-mode-setup ()
    ;; Always hilight the current agenda line
    (hl-line-mode 1))

  (add-hook 'org-agenda-mode-hook #'nwg/org-agenda-mode-setup 'append)

  (nwg/install-custom-org-links)

  (defun II () "  ")

  (defun nwg/fmt-clk (clock-link)
    (if org-clock-current-task
        (format "%sCurrent Task: %s\n" (nwg/I) clock-link)
      ""))

  (nwg/install-org-capture-exit-option)
  ;; Set up post-capture actions in nwg-exit-templates-alist
  ;; car of each item be pointed to by custom `:exit` property in normal
  ;; org-capture-templates
  (let ((post-todo '(entry (file+olp+datetree journal-file)
                           "* Created new TODO: %a\n%(nwg/fmt-clk \"%K\")%(nwg/I)%U\n"
                           :immediate-finish t)))

    (setq nwg-exit-templates-alist
          `(("log-todo-to-journal" . ,post-todo))))

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
