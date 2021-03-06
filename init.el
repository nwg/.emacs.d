;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; (setq debug-on-error t)
;; (setq debug-ignored-errors nil)

(require 'cl-lib)

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
(use-package nwg-util
  :demand t)

(when (featurep 'nwg-util)
  (load-library "nwg-util"))

(defun sync-file (&rest args)
  (apply #'f-join user-emacs-directory "sync" args))

;; Main Initialization

(defun enable-parens ()
  (setq show-paren-delay 0)
  (set-face-foreground 'show-paren-mismatch "red")
  (set-face-attribute 'show-paren-mismatch nil
                      :weight 'bold :underline t :overline nil :slant 'normal)

  (set-face-background 'show-paren-match "#aaaaaa")
  (set-face-attribute 'show-paren-match nil
                      :weight 'bold :underline nil :overline nil :slant 'normal)

  (show-paren-mode t)
  (setq-local show-paren-style 'expression))

(add-hook 'lisp-mode-hook #'enable-parens)

(setq inhibit-splash-screen t)

;; Recentf
(setq recentf-save-file (sync-file "recentf"))
(recentf-mode 1)

;; Customization setup
(setq custom-git-file (f-join user-emacs-directory "custom-git.el"))
(when (file-exists-p custom-git-file)
  (load-file custom-git-file))

(setq custom-file (sync-file "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Quick isearch mode <return>
(defun nwg/isearch-quick-return ()
  (interactive)
  (when isearch-mode (isearch-done))
  (let ((fn (or (local-key-binding (kbd "<return>")) (local-key-binding (kbd "RET")))))
    (when fn
      (funcall fn))))

(define-key isearch-mode-map (kbd "M-<return>") #'nwg/isearch-quick-return)
(define-key isearch-mode-map (kbd "M-RET") #'nwg/isearch-quick-return)

; I use "s-F" and it has a bug (https://emacs.stackexchange.com/questions/64969/mac-os-and-possibly-other-guis-why-is-default-s-g-behaving-differently-from)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-<return>") #'nwg/isearch-quick-return)
  (define-key dired-mode-map (kbd "M-RET") #'nwg/isearch-quick-return))

(global-set-key (kbd "C-M-s-m") #'describe-mode)

(when (not (featurep 'init))
  (setq nwg/initial-profile-path (nwg/user-dot-profile-path)))

;; Set up PATH and 'exec-path
(let* ((additions '("~/.nix-profile/bin" "~/.local/bin" "/Library/TeX/texbin" "/opt/local/bin" "/Applications/Racket v8.1/bin"))
       (additions-expanded (mapcar #'expand-file-name additions))
       (new-environment-path (nwg/prepend-if-missing additions-expanded nwg/initial-profile-path))
       (new-exec-path (nwg/prepend-if-missing additions-expanded exec-path)))
  (nwg/set-environment-path new-environment-path)
  (setq exec-path new-exec-path))

;; Set up ibuffer
;;  Switch to the "Standard" View on entry and collapse "Default"
(defun nwg/ibuffer-mode()
  (ibuffer-switch-to-saved-filter-groups "Standard")
  (setq ibuffer-hidden-filter-groups (list "Default"))
  (ibuffer-update nil t))

(add-hook 'ibuffer-mode-hook #'nwg/ibuffer-mode)
(global-set-key (kbd "C-x C-b") #'ibuffer)

; Mac OS X Cmd-click maps to middle mouse
(when (eq system-type 'darwin)
  (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>")))

; Highlight whitespace
(setq-default whitespace-style '(face trailing tabs spaces))
(global-auto-revert-mode t)

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

(setq nwg/org-file-re "\\.org$")
(setq nwg/backup-file-re "^\\.#")

(fset #'backup-file-p (compose (curry #'s-matches\? nwg/backup-file-re) #'f-filename))

(fset #'nwg/orgs-recursively
      (compose
       (curry #'cl-remove-if #'backup-file-p)
       (rcurry #'directory-files-recursively nwg/org-file-re)))

(defun nwg/scan-notes-dir ()
  (and
   (file-directory-p notes-dir)
   (nwg/orgs-recursively notes-dir)))

(defvar notes-files (nwg/scan-notes-dir))

(defun nwg/setup-notes ()
  (setq notes-files (nwg/scan-notes-dir))
  (when (featurep 'org-agenda)
    (org-store-new-agenda-file-list notes-files)))

(defun nwg/notes-file-p (fn)
  (and
   (f-ancestor-of\? notes-dir fn)
   (s-matches\? nwg/org-file-re fn)
   (not (s-matches\? nwg/backup-file-re fn))))

(defun nwg/maybe-rescan-notes ()
  (when (nwg/notes-file-p buffer-file-name)
    (nwg/rescan-notes)))

(defun nwg/maybe-add-note ()
  (when (and
         (nwg/notes-file-p buffer-file-name)
         (file-exists-p buffer-file-name)
         (not (member buffer-file-name notes-files)))
    (message "Adding note %s to notes and agenda" buffer-file-name)
    (add-to-list 'notes-files buffer-file-name)
    (when (featurep 'org)
      (setq org-agenda-files notes-files))))

(add-hook 'after-save-hook #'nwg/maybe-add-note)
(add-hook 'find-file-hook #'nwg/maybe-add-note)

(defun nwg/recentf-exclude-org ()
  (message "excluding org files from recentf")
  (setq-local recentf-exclude '("\\.org")))

(add-hook 'org-agenda-mode-hook #'nwg/recentf-exclude-org)
(add-hook 'dashboard-mode-hook #'nwg/recentf-exclude-org)

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
(global-set-key (kbd "C-M-s-\\") (lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "C-s-\\") (lambda () (interactive) (counsel-bookmark)))
(global-set-key (kbd "C-c j") (lambda () (interactive) (org-capture '(4) "j")))
(global-set-key (kbd "C-x w o") 'window-swap-states)
(global-set-key (kbd "C-x w -") #'nwg/move-buffer-to-previous-frame)

(global-set-key (kbd "s-k") #'erase-buffer)

(defun copy-current-sexp ()
  (interactive)
  (save-excursion
    (let ((start (progn (backward-sexp) (pos)))
          (end (progn (forward-sexp) (pos))))
      (copy-region-as-kill start end))))

; Some global bindings for text editing
(global-set-key (kbd "C-^") (?? () (interactive) (delete-indentation 4))) ; Like M-^ but execute from top

; Recent file keys
(cl-loop for i from 0 to 9
      do (let ((keys (format "C-x r %d" i))
               (sym (format "recentf-open-most-recent-file-%d" i)))
           (global-set-key (kbd keys) (intern sym))))

(use-package bookmark
  :custom
  (bookmark-file (sync-file "bookmarks")))

(use-package lsp-mode
  :straight t)

(use-package nix-mode
  :straight t
  :after (lsp-mode)
  :bind (:map nix-mode-map ("M-'" . #'nix-doc))
  :config

  (defun nix-doc (&optional key)
    (interactive)
    (let ((key (or
                key
                (and (called-interactively-p 'any) (prompt-current-word "nix-doc: "))
                (error "Cannot determine key"))))
      (shell-command (format "nix-doc %s" key))))

  (add-hook 'nix-mode-hook #'enable-parens)
  (add-hook 'nix-mode-hook #'lsp)

  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package ggtags
  :straight t
  :init
  (defun enable-ggtags ()
    (with-eval-after-load 'xref
      (add-hook 'xref-backend-functions #'ggtags--xref-backend -10 t)))

  (add-hook 'prog-mode-hook #'enable-ggtags))

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
  :after ivy
  :bind (("C-x r m" . counsel-bookmark)))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :after (ivy prescient)
  :custom
  (ivy-prescient-retain-classic-highlighting t "Use the old ivy highlighting style")
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :straight t
  :after (company prescient)
  :config
  (company-prescient-mode 1))

(use-package solarized-theme
  :straight t
  :config
  (when window-system
    (load-theme 'solarized-light t)))

(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              ("s-<up>" . dired-up-directory)
              ("s-<down>" . dired-find-file))
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
    (let* ((buf (current-buffer)))
      (call-interactively #'find-file)))

  (define-key dired-mode-map (kbd "n") #'nwg/dired-new-file)

  (defun nwg/setup-dired ()
    (dired-omit-mode -1))

  (add-hook 'dired-mode-hook #'nwg/setup-dired))

(require 'dired-x)

(use-package racket-mode
  :straight t
  :custom
  (racket-program "/Users/griswold/project/racket/racket/bin/racket")
  :config
  (require 'racket-xp)

  (defun nwg/racket-mode ()
    (racket-xp-mode 1)
    (show-paren-mode 1))

  (add-hook 'racket-mode-hook #'nwg/racket-mode)
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
  (org-refile-use-outline-path 'full-file-path "Show file and full node paths for refiling")
  (org-refile-allow-creating-parent-nodes 'confirm "Autocreate parents with confirmation")
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
  (global-set-key (kbd "C-c C-w") 'org-refile)

  (define-key org-mode-map (kbd "C-c C-q") #'counsel-org-tag)

  (org-store-new-agenda-file-list notes-files)

  (defun nwg/clock-in-prepare-hook ()
    (org-id-get-create))

  (add-hook 'org-clock-in-prepare-hook #'nwg/clock-in-prepare-hook)

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
                  "\n* TODO %?\n%(II)%U%(II)Context: %a\n" :clock-in t :clock-resume t :exit finish-todo)
                 ("r" "respond" entry (file org-default-notes-file)
                  "* NEXT Respond to %:from on %:subject\n%(II)SCHEDULED: %t\n%(II)%U\n%(II)%a\n" :clock-in t :clock-resume t :immediate-finish t)
                 ("n" "note" entry (file org-default-notes-file)
                  "* %? :note:\n%(II)%U\n%(II)Context: %a\n" :clock-in t :clock-resume t :exit finish-note)
                 ("e" "email" entry (file org-default-notes-file)
                  "* %? :email:\n%(II)%U\n%(II)Context: %a\n" :clock-in t :clock-resume t)
                 ("j" "Journal" entry (file+olp+datetree journal-file)
                  "* %?\n%(II)Context: %a\n%(II)%U\n" :clock-in t :clock-resume t)
                 ("w" "org-protocol" entry (file org-default-notes-file)
                  "* TODO Review %c\n%(II)%U\n" :immediate-finish t)
                 ("m" "Meeting" entry (file org-default-notes-file)
                  "* MEETING with %? :MEETING:\n%(II)%U" :clock-in t :clock-resume t)
                 ("p" "Phone call" entry (file org-default-notes-file)
                  "* PHONE %? :PHONE:\n%(II)%U" :clock-in t :clock-resume t :exit finish-phone)
                 ("h" "Habit" entry (file org-default-notes-file)
                  "* NEXT %?\n%(II)%U\n%(II)Context: %a\n%(II)SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n%(II):PROPERTIES:\n%(II):STYLE: habit\n%(II):REPEAT_TO_STATE: NEXT\n%(II):END:\n")
                 ("L" "Link" entry (file org-default-notes-file)
                  "* %? :web:\n%(II)%U\n%(II)Link: %a\n" :clock-in t :clock-resume t)
                 ("S" "Link Selected" entry (file org-default-notes-file)
                  "* %? :web:\n%(II)%U\n%(II)Link: %a\n%(II)#+BEGIN_QUOTE\n%(II)%i\n%(II)#+END_QUOTE\n" :clock-in t :clock-resume t)
                                        ;"* %? :web:\n%(II)%U\n%(II)Link: %a\n%i\n"
                 )))

  (defun nwg/org-agenda-mode-setup ()
    ;; Always hilight the current agenda line
    (hl-line-mode 1))

  (add-hook 'org-agenda-mode-hook #'nwg/org-agenda-mode-setup 'append)

  (nwg/install-custom-org-links)

  (defun II () "  ")

  (defun nwg/fmt-clk (clock-link)
    (if org-clock-current-task
        (format "%sCurrent Task: %s\n" (II) clock-link)
      ""))

  (nwg/install-org-capture-exit-option)
  ;; Set up post-capture actions in nwg-exit-templates-alist
  ;; car of each item be pointed to by custom `:exit` property in normal
  ;; org-capture-templates
  (defun nwg/note-capture-entry (prefix)
    `(entry (file+olp+datetree journal-file)
                           ,(format "* %s%%a\n%%(nwg/fmt-clk \"%%K\")%%(II)%%U\n" prefix)
                           :immediate-finish t))

  (setq nwg-exit-templates-alist
        `((finish-todo . ,(nwg/note-capture-entry "Created new TODO: "))
          (finish-note . ,(nwg/note-capture-entry "Created new NOTE: "))
          (finish-phone . ,(nwg/note-capture-entry ""))))
  )

(use-package ripgrep
  :straight t)

(use-package projectile-ripgrep
  :straight t
  :after ripgrep)

(use-package projectile
  :straight t
  :after (projectile-ripgrep)
  :custom
  (projectile-tags-backend 'xref "Always use xref. Projectile prefers ggtags by default but it supports xref, anyway")
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

)

(use-package all-the-icons
  :straight t)

(use-package dashboard
  :straight (dashboard :type git :host github :repo "emacs-dashboard/emacs-dashboard")
  :after all-the-icons
  :init
  ;; (message "recentf-list=%s" recentf-list)
  :config
  (message "startup recentf-list=%s" recentf-list)
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

  (setq dashboard-items '((bookmarks . 10)
                          (recents  . 5)
                          (agenda . 5)
                          (projects . 7)
                          (registers . 5)))

)

(defvar startup-scratch-banner "~/.emacs.d/tree.png")

(defun add-image ()
  (with-current-buffer (get-buffer "*scratch*")
    (goto-char (point-min))
    (nwg/insert-image startup-scratch-banner '(read-only t))
    (insert "\n\n")
    (goto-char (point-max))))

(defun maybe-add-image ()
  (unless (or (boundp 'nwg/did-add-image) (not window-system))
    (add-image)
    (setq nwg/did-add-image t)))

(add-hook 'emacs-startup-hook #'maybe-add-image)

;; (require 'org-protocol)
(require 'server)

;; (add-hook 'server-visit-hook (?? () (require 'org-protocol)) -90)
(unless (server-running-p) (server-start))
(provide 'init)
(put 'erase-buffer 'disabled nil)
