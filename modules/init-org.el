;;;; Org mode

(require 'init-prefs)
(use-package org
  :commands (org-mode)
  :mode (("\\.org\\'" . org-mode))
  :init
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (defface exordium-org-wait '((t (:inherit org-todo)))
    "Face for WAIT keywords."
    :group 'exordium)
  (defface exordium-org-work '((t (:inherit org-todo)))
    "Face for WORK keywords."
    :group 'exordium)

  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "WORK(w!/!)" "WAIT(a@/!)" "|" "DONE(d!/!)")))
  (org-todo-keyword-faces
   '(("WORK" . exordium-org-work)
     ("WAIT" . exordium-org-wait)))
  (org-startup-truncated nil)
  (org-startup-with-inline-images t)
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-src-preserve-indentation t)
  ;; Turn off the confirmation for code eval when using org-babel
  (org-confirm-babel-evaluate (not exordium-no-org-babel-confirm))
  :config
  (when exordium-enable-org-export
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
