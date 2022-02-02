;;;; Org mode

(require 'init-prefs)
(use-package org
  :commands (org-mode)
  :mode (("\\.org\\'" . org-mode))
  :init
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  :config
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO" "WORK" "WAIT" "DONE")))
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)
  (setq org-src-preserve-indentation t)
  (setq org-completion-use-ido t)
  (when exordium-enable-org-export
    ;; Turn off the confirmation for code eval when using org-babel
    (when exordium-no-org-babel-confirm
      (setq org-confirm-babel-evaluate nil))

    ;; Configure export using a css style sheet
    (when exordium-org-export-css
      (setq org-html-htmlize-output-type 'css)
      (setq org-html-head exordium-org-export-css-stylesheet)))

  (setq org-support-shift-select t)
  (when exordium-enable-org-export
    ;; Enable org-babel for perl, ruby, sh, python, emacs-lisp, C, C++, etc
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((perl       . t)
       (ruby       . t)
       (shell      . t)
       (python     . t)
       (emacs-lisp . t)
       (C          . t)
       (dot        . t)))))



;;; Show org-mode bullets as UTF-8 characters.
(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

;;; visual line mode in org-mode, paragraphs without embedded newline

;; use ido for org completion

(use-package ox-html
  :ensure org
  :after (org))

(use-package ox-md
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-beamer
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-odt
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-publish
  :ensure org
  :after (org)
  :if exordium-enable-org-export)

(use-package ox-gfm
  :ensure t
  :after (org)
  :if exordium-enable-org-export)


(provide 'init-org)
