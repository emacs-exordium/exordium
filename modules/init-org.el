;;;; Org mode

(require 'init-prefs)
(require 'org)
(eval-when-compile
  (require 'ox-html)
  (require 'fill-column-indicator))

(setq org-todo-keywords
      '((sequence "TODO" "WORK" "WAIT" "DONE")))
(setq org-startup-truncated nil)

;;; Native formatting in code blocks
(setq org-src-fontify-natively t)

;;; Preserve indentation in code blocks
(setq org-src-preserve-indentation t)

;;; Show images inline
(setq org-startup-with-inline-images t)

;;; Show org-mode bullets as UTF-8 characters.
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; visual line mode in org-mode, paragraphs without embedded newline
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; use ido for org completion
(setq org-completion-use-ido t)

(when exordium-enable-org-export
  ;; load exporters for markdown, beamer, ODT, and site publish
  (eval-after-load "org"
    '(progn
       (require 'ox-md)                 ; markdown
       (require 'ox-beamer)             ; beamer (LaTeX slides)
       (require 'ox-odt)                ; open doc format
       (require 'ox-publish)            ; publish web sites
       (require 'ox-gfm)))              ; github-flavored-markdown

  ;; Enable org-babel for perl, ruby, sh, python, emacs-lisp, C, C++, etc
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((perl       . t)
     (ruby       . t)
     ,(if (version< org-version "9.0")
         '(sh         . t)
       '(shell      . t))
     (python     . t)
     (emacs-lisp . t)
     (C          . t)
     (dot        . t)))

  ;; Turn off the confirmation for code eval when using org-babel
  (when exordium-no-org-babel-confirm
    (setq org-confirm-babel-evaluate nil))

  ;; Configure export using a css style sheet
  (when exordium-org-export-css
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-head exordium-org-export-css-stylesheet)))

;;; Don't markup src blocks with fill column indicator
(add-hook 'org-src-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

(provide 'init-org)
