(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   '(("Standard"
      ("Modified"
       (visiting-file)
       (modified))
      ("Projectile Files"
       (saved . "projectile files"))
      ("Programming"
       (saved . "programming")))))
 '(ibuffer-saved-filters
   '(("projectile files"
      (predicate not
                 (member
                  (ibuffer-buffer-file-name)
                  org-agenda-files))
      (predicate . projectile-mode)
      (visiting-file))
     ("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(org-agenda-files
   '("/Users/griswold/Documents/Notes/Journal/Journal.org" "/Users/griswold/Documents/Notes/My Software/Editor Buddy/Design.org" "/Users/griswold/Documents/Notes/Network/OpenWrt/OpenWrt.org" "/Users/griswold/Documents/Notes/Network/ddwrt/R6400v2/r6400v2-flash.org" "/Users/griswold/Documents/Notes/Network/Setup.org" "/Users/griswold/Documents/Notes/Paperwork/Spectrum/Spectrum.org" "/Users/griswold/Documents/Notes/Physics/Resources.org" "/Users/griswold/Documents/Notes/Physics/particle.org" "/Users/griswold/Documents/Notes/Privacy/Privacy and the Web.org" "/Users/griswold/Documents/Notes/Software/Emacs/Emacs.org" "/Users/griswold/Documents/Notes/Software/Firefox/Firefox.org" "/Users/griswold/Documents/Notes/Software/Linux/Linux.org" "/Users/griswold/Documents/Notes/Software/OSX/Services.org" "/Users/griswold/Documents/Notes/Software/OSX/Time Machine.org" "/Users/griswold/Documents/Notes/Software/Racket/Racket.org" "/Users/griswold/Documents/Notes/Software/Sieve/Sieve.org" "/Users/griswold/Documents/Notes/Software/Something New/Blah.org" "/Users/griswold/Documents/Notes/Software/Universal Ctags/Universal Ctags.org" "/Users/griswold/Documents/Notes/Software/NewThing.org" "/Users/griswold/Documents/Notes/Software/NewThing2.org" "/Users/griswold/Documents/Notes/Software/Software Notes.org" "/Users/griswold/Documents/Notes/Unfiled/Unfiled.org") nil nil "Customized with use-package org")
 '(safe-local-variable-values
   '((eval setq-local racket-program
           (f-join
            (projectile-project-root)
            "racket/bin/racket"))
     (racket-user-command-line-arguments "-I" "racket"))))
