;;;; Org mode

(require 'init-prefs)

(defface exordium-org-wait '((t (:inherit org-todo)))
  "Face for WAIT keywords."
  :group 'exordium)
(defface exordium-org-work '((t (:inherit org-todo)))
  "Face for WORK keywords."
  :group 'exordium)
(defface exordium-org-stop '((t (:inherit org-done)))
  "Face for STOP keywords."
  :group 'exordium)

(exordium-ignore-builtin 'org)

(use-package org
  :commands (org-mode)
  :mode (("\\.org\\'" . org-mode))
  :bind
  (:map org-mode-map
        ([remap org-toggle-comment] . iedit-mode))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "WORK(w!/!)" "WAIT(a@/!)" "|" "STOP(s@/!)" "DONE(d!/!)")))
  (org-todo-keyword-faces
   '(("WORK" . exordium-org-work)
     ("WAIT" . exordium-org-wait)
     ("STOP" . exordium-org-stop)))
  (org-startup-folded t)
  (org-log-into-drawer t)
  (org-startup-truncated nil)
  (org-startup-with-inline-images t)
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate (not exordium-no-org-babel-confirm)
                              "Turn off the confirmation for code eval when using org-babel.")
  (org-support-shift-select t)
  :config
  (add-hook 'org-src-mode-hook
            #'(lambda ()
                (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)

  (defun exordium--org-babel-after-execute ()
    "Redisplay inline images in subtree if cursor in source block with :result graphics.

Rationale:
For some reason `org-babel-execute' is not producing images from .dot format (`org-version' 9.5.4).
This is a spin off https://stackoverflow.com/a/66911315/519827, but REFRESH is set to nil."
    (when (org-in-src-block-p)
      (let (beg end)
        (save-excursion
          (org-mark-subtree)
          (setq beg (point))
          (setq end (mark)))
        (when-let ((info (org-babel-get-src-block-info t))
                   (params (org-babel-process-params (nth 2 info)))
                   (result-params (cdr (assq :result params)))
                   ((string-match-p "graphics" result-params)))
          (org-display-inline-images nil nil beg end)))))
  (add-hook 'org-babel-after-execute-hook #'exordium--org-babel-after-execute)

  ;; TODO: delete `exordium-enable-org-export'??
  (when exordium-enable-org-export
    ;; Enable org-babel for perl, ruby, sh, python, emacs-lisp, C, C++, etc
    ;; TODO: add extra languages configurable by user
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((perl       . t)
       (ruby       . t)
       (shell      . t)
       (python     . t)
       (emacs-lisp . t)
       (C          . t)
       (dot        . t)
       (sql        . t)))))


;;; Show org-mode bullets as UTF-8 characters.
(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package ox-html
  :ensure org
  :after (org))

(use-package ox-html
  :ensure org
  :after (org)
  :if exordium-org-export-css
  :custom
  (org-html-htmlize-output-type 'css
                                "Configure export using a css style sheet")
  (org-html-head exordium-org-export-css-stylesheet
                 "Configure export using a css style sheet"))

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
