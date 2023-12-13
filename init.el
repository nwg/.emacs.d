
; straight.el bootstrapping (copy-pasted from website)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package f
  :straight t)

(setq custom-file (f-join user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package lsp-mode
  :straight t
  :bind-keymap
  ("C-s-l" . lsp-command-map)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-keymap-prefix "C-s-l"))

(use-package nix-mode
  :straight t
  :after (lsp-mode)
  :bind (:map nix-mode-map
	      ("C-c h N" . #'nix-doc))
  :config

  (defun nix-doc (&optional key)
    (interactive)
    (let ((key (or
                key
                (and (called-interactively-p 'any) (prompt-current-word "Describe nix: "))
                (error "Cannot determine key"))))
      (shell-command (format "nix-doc %s" key))))

  (add-hook 'nix-mode-hook #'nwg/enable-parens)
;  (add-hook 'nix-mode-hook #'lsp)

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
  
(use-package projectile
  :straight t

  :demand t
  
  :bind-keymap
  ("C-s-p" . projectile-command-map)
  
  :custom
  (projectile-tags-backend 'xref "Always use xref. Projectile prefers ggtags by default but it supports xref, anyway")
  
  :config
  (setq projectile-project-search-path '("~/Project/"))
  (projectile-discover-projects-in-search-path)
  
  :bind (:map projectile-mode-map
	      ("s-p" . #'projectile-command-map)
	      ("C-c p" . #'projectile-command-map)))

(global-set-key (kbd "C-x C-\\") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-x e") (lambda () (interactive) (eval-buffer)))

(add-to-list 'load-path (f-join user-emacs-directory "library/nwg-util/lisp"))
(use-package nwg-util
  :demand t)
