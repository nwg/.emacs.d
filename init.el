(let ((my-path (expand-file-name "/Library/TeX/texbin:/opt/local/bin")))
  (setenv "PATH" (concat my-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-path))

(global-auto-revert-mode t)
(global-display-line-numbers-mode)
;; Dsetqon't pop up new gui window for files opened from cmdline
(setq ns-pop-up-frames nil)
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

(customize-set-variable
 'display-buffer-base-action
 '((display-buffer-same-window display-buffer-reuse-window
    display-buffer-in-previous-window
    display-buffer-use-some-window)))

(customize-set-variable
 'display-buffer-alist
 (cons '("^CAPTURE-.*\\.org$" (display-buffer-same-window display-buffer-reuse-window))
       display-buffer-alist))

(defun my-backward-delete (arg)
  (interactive "P")
  (message "here")
  (funcall-interactively 'ivy-backward-delete-char)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-on-del-error-function #'ivy-immediate-done)
 '(lispy-backward-delete-char-fn 'my-backward-delete)
 '(lispy-backward-delete-char-untabify-fn 'my-backward-delete)
 '(safe-local-variable-values '((TeX-command-extra-options . "-shell-escape"))))


;; (use-package lispy
;;   :config)

;; (add-hook 'emacs-lisp-mode-hook 'lispy-mode)

(use-package solarized-theme :straight t)
(load-theme 'solarized-light t)
;; (add-hook 'after-change-major-mode-hook 'superword-mode)

;; (add-hook
;;  'ivy-mode-hook
;;  (lambda ()
;;    (message "here in ivy-mode hook")))

(use-package ivy
  :straight t
  ;; :after lispy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (message (format "%s" ivy-backward-delete-char))
  )

;; (eval-after-load 'ivy
;;   '(progn
;;     (setq lispy-backward-delete-char-fn 'ivy-backward-delete-char)
;;     (setq lispy-backward-delete-char-untabify-fn 'ivy-backward-delete-char))
;;   )

(setq-default tab-always-indent 'complete)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   ;; Use Company for completion
;;   :bind (("C-<tab>" . company-complete-common)
;;          :map company-mode-map
;;          ([remap completion-at-point] . company-complete-common)
;;          ([remap complete-symbol] . company-complete-common))
;;   :init (global-company-mode 1)
;;   :config
;;   (setq tab-always-indent 'complete)
;;   ;; some better default values
;;   (setq company-idle-delay 0.2)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-selection-wrap-around t)

;;   ;; align annotations in tooltip
;;   (setq company-tooltip-align-annotations t)

;;   ;; nicer keybindings
;;   (define-key company-active-map (kbd "C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

;;   ;; put most often used completions at stop of list
;;   (setq company-transformers '(company-sort-by-occurrence)))


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(lispy-backward-delete-char-fn 'ivy-backward-delete-char)
;; '(lispy-backward-delete-char-untabify-fn 'ivy-backward-delete-char))
;; (custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; )
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
            

;; (load "popup-complete")

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda ()
;; 	    (setq complete-in-region-use-popup t)))

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
  
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (setq tab-always-indent nil)
     ;; (setq indent-line-function 'insert-tab)
     (setq indent-line-function 'indent-relative-first-indent-point)))
	      
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
		)))

(use-package org
  :straight t
  :config
  (setq org-support-shift-select t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package all-the-icons
  :straight t)

(use-package helm-descbinds
  :straight t
  :bind
   ("C-h b" . helm-descbinds))

(use-package helm
  :straight t
  :config
  (helm-mode 1)
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*helm" (* not-newline) "*" eos)
  ;;                (display-buffer-in-side-window)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 0.4)))
  (setq helm-bookmark-show-location t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x r l" . helm-bookmarks)
   ("C-x C-b" . helm-buffers-list)
   ("M-s o" . helm-occur)
   ("C-h a" . helm-apropos)))

(use-package dashboard
  :straight t
  :after all-the-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner "~/.emacs.d/tree.png"))

(toggle-debug-on-error)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)
