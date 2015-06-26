;;; color-theme-solarized.el -- a low contrast theme.
;;;
;;; Credit:
;;; Solarized colors are from Ethan Schoonover.
;;; See http://ethanschoonover.com/solarized
;;; Greg Pfeil created a theme for Emacs. This file is a different
;;; implementation but uses the same choices for most faces.

(require 'init-prefs)
(require 'org)
(require 'cl-lib)

;;; Color palette

(defconst solarized-colors
  '((base03  . "#002b36")
    (base02  . "#073642")
    (base01  . "#586e75")
    (base00  . "#657b83")
    (base0   . "#839496")
    (base1   . "#93a1a1")
    (base2   . "#eee8d5")
    (base3   . "#fdf6e3")
    (yellow  . "#b58900")
    (orange  . "#cb4b16")
    (red     . "#dc322f")
    (magenta . "#d33682")
    (violet  . "#6c71c4")
    (blue    . "#268bd2")
    (cyan    . "#2aa198")
    (green   . "#859900")))

;;; Theme definition

(defmacro with-solarized-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the solazized colors.
`MODE' is 'dark or 'light."
  `(let ((class '((class color) (min-colors 89)))
         (back (cdr (assoc (if (eq ,mode 'light) 'base3 'base03)
                           solarized-colors)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   solarized-colors))
     (when (eq ,mode 'light)
       ;; swap the base colors
       (cl-rotatef base03 base3)
       (cl-rotatef base02 base2)
       (cl-rotatef base01 base1)
       (cl-rotatef base00 base0))
     ,@body))

(defmacro solarized-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((,class (:foreground ,base0 :background ,back))))
     (shadow ((,class (:background ,base01))))
     (success ((,class (:foreground ,green))))
     (error ((,class (:weight bold :foreground ,red))))
     (warning ((,class (:weight bold :foreground ,orange))))
     (scroll-bar ((,class (:background ,back))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:weight bold :foreground ,cyan :background ,base02))))
     (show-paren-mismatch ((,class (:weight bold :foreground ,red :background ,base01))))

     ;; Region
     (region ((,class (:background ,base00))))
     (secondary-selection ((,class (:background ,base02))))

     ;; font-lock
     (font-lock-builtin-face ((,class (:foreground ,green :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,base01 :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,cyan))))
     (font-lock-function-name-face ((,class (:foreground ,blue))))
     (font-lock-keyword-face ((,class (:foreground ,green))))
     (font-lock-string-face ((,class (:foreground ,cyan))))
     (font-lock-type-face ((,class (:foreground ,yellow))))
     (font-lock-variable-name-face ((,class (:foreground ,blue))))
     (font-lock-warning-face ((,class (:foreground ,red :weight bold))))
     (font-lock-doc-face ((,class (:foreground ,base01 :slant italic))))
     (font-lock-doc-string-face ((,class (:foreground ,base01 :slant italic))))
     (font-lock-color-constant-face ((,class (:foreground ,green))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,base01 :slant italic))))
     (font-lock-preprocessor-face ((,class (:foreground ,orange))))
     (font-lock-reference-face ((,class (:foreground ,cyan))))
     (font-lock-negation-char-face ((,class (:foreground ,red))))
     (font-lock-other-type-face ((,class (:foreground ,blue :slant italic))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,orange))))
     (font-lock-special-keyword-face ((,class (:foreground ,red))))
     (font-lock-exit-face ((,class (:foreground ,red))))
     (font-lock-other-emphasized-face ((,class (:foreground ,violet))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))

     ;; Emacs interface
     (cursor ((,class (:foreground ,base03 :background ,base0))))
     (escape-glyph-face ((,class (:foreground ,red))))
     (fringe ((,class (:foreground ,base01 :background ,base02))))
     (linum ((,class (:foreground ,base01 :background ,base02))))
     (header-line ((,class (:foreground ,base0 :background ,base02 :weight bold))))
     (highlight ((,class (:background ,base02))))
     (hl-line ((,class (:background ,base02))))
     (menu ((,class (:foreground ,base0 :background ,base02))))
     (link ((,class (:foreground ,violet :underline t))))
     (link-visited ((,class (:foreground ,magenta :underline t))))
     (vertical-border ((,class (:foreground ,base0))))

     ;; Mode line
     (minibuffer-prompt ((,class (:weight bold :foreground ,cyan))))
     (mode-line ((,class (:foreground ,base1 :background ,base01 :box nil))))
     (mode-line-inactive ((,class (:foreground ,base00 :background ,base02 :box nil))))

     ;; Powerline
     (exordium-powerline-active1 ((,class (:background ,base01 :foreground ,back))))
     (exordium-powerline-active2 ((,class (:background ,base02 :foreground ,base01))))
     (exordium-powerline-active3 ((,class (:background ,base2 :foreground ,back))))
     (exordium-powerline-active4 ((,class (:background ,red :foreground ,back))))
     (exordium-powerline-active5 ((,class (:background ,green :foreground ,back))))
     (exordium-powerline-inactive1 ((,class (:background ,base02))))
     (exordium-powerline-inactive2 ((,class (:background ,base02))))
     (exordium-powerline-inactive3 ((,class (:background ,base00 :foreground ,back))))
     (exordium-project-name ((,class (:background ,violet :foreground ,back))))

     ;; Search
     (isearch ((,class (:foreground ,orange :background ,back))))
     (isearch-fail ((,class (:foreground ,orange :background ,back))))
     (lazy-highlight ((,class (:foreground ,yellow :background ,back))))
     (match ((,class (:foreground ,yellow :background ,back))))

     ;; Compilation
     (compilation-info ((,class (:weight bold :foreground ,green))))
     (compilation-warning ((,class (:weight bold :foreground ,orange))))

     ;; Custom
     (custom-button
      ((,class (:foreground ,base1 :background ,base02
           :box (:line-width 2 :style released-button)))))
     (custom-button-mouse
      ((,class (:foreground ,base1 :background ,base02 :inherit custom-button))))
     (custom-button-pressed
      ((,class (:foreground ,base1 :background ,base02
           :box (:line-width 2 :style pressed-button)
           :inherit custom-button-mouse))))
     (custom-changed ((,class (:foreground ,blue :background ,base3))))
     (custom-comment ((,class (:foreground ,base1 :background ,base02))))
     (custom-comment-tag ((,class (:foreground ,base1 :background ,base02))))
     (custom-documentation ((,class (:inherit default))))
     (custom-group-tag ((,class (:foreground ,base1))))
     (custom-group-tag-1 ((,class (:weight bold :foreground ,base1))))
     (custom-invalid ((,class (:foreground ,red :background ,back))))
     (custom-link ((,class (:foreground ,violet))))
     (custom-state ((,class (:foreground ,green))))
     (custom-variable-tag ((,class (:foreground ,base1))))

     ;; Info
     (info-xref ((,class (:foreground ,blue))))
     (info-xref-visited ((,class (:foreground ,magenta :inherit info-xref))))

     ;; RTags
     (rtags-errline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,red :foreground ,back)
                                `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,orange :foreground ,back)
                                 `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,green :foreground ,back)
                                  `(:underline (:color ,green :style wave))))))

     ;; Org
     (org-hide ((,class (:foreground ,base03))))
     (org-todo ((,class (:weight bold :foreground ,base03 :background ,red))))
     (org-done ((,class (:weight bold :foreground ,green))))
     (org-todo-kwd-face ((,class (:foreground ,red :background ,base03))))
     (org-done-kwd-face ((,class (:foreground ,green :background ,base03))))
     (org-project-kwd-face ((,class (:foreground ,violet :background ,base03))))
     (org-waiting-kwd-face ((,class (:foreground ,orange :background ,base03))))
     (org-someday-kwd-face ((,class (:foreground ,blue :background ,base03))))
     (org-started-kwd-face ((,class (:foreground ,yellow :background ,base03))))
     (org-cancelled-kwd-face ((,class (:foreground ,green :background ,base03))))
     (org-delegated-kwd-face ((,class (:foreground ,cyan :background ,base03))))
     (org-document-title ((,class
                           ,(append `(:weight bold :foreground ,cyan)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))
     (org-level-1 ((,class
                    ,(append `(:foreground ,base0)
                             (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))

     ;; outline
     (outline-1 ((,class (:foreground ,blue))))
     (outline-2 ((,class (:foreground ,cyan))))
     (outline-3 ((,class (:foreground ,yellow))))
     (outline-4 ((,class (:foreground ,red))))
     (outline-5 ((,class (:foreground ,base0))))
     (outline-6 ((,class (:foreground ,base01))))
     (outline-7 ((,class (:foreground ,orange))))
     (outline-8 ((,class (:foreground ,violet))))

     ;; Flymake
     (flymake-errline ((,class (:weight bold :foreground ,red :background ,back))))
     (flymake-warnline ((,class (:weight bold :foreground ,orange :background ,back))))

     ;; git-gutter
     (git-gutter:modified ((,class (:foreground ,violet))))
     (git-gutter:added ((,class (:foreground ,green))))
     (git-gutter:deleted ((,class (:foreground ,red))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,red))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,orange))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,magenta))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,base0))))

     ;; slime
     (slime-error-face ((,class (:foreground ,red))))
     (slime-note-face ((,class (:foreground ,yellow))))
     (slime-repl-inputted-output-face ((,class (:foreground ,red))))
     (slime-repl-output-mouseover-face ((,class (:box (:color ,base3)))))
     (slime-style-warning-face ((,class (:weight bold :foreground ,orange))))
     (slime-warning-face ((,class (:weight bold :foreground ,red))))

     ;;flyspell
     (flyspell-incorrect ((,class (:foreground ,red))))
     (flyspell-duplicate ((,class (:foreground ,yellow))))

     ;;ansi-term
     (term-color-black ((,class ( :foreground ,base02))))
     (term-color-red ((,class ( :foreground ,red))))
     (term-color-green ((,class ( :foreground ,green))))
     (term-color-yellow ((,class ( :foreground ,yellow))))
     (term-color-blue ((,class ( :foreground ,blue))))
     (term-color-magenta ((,class ( :foreground ,magenta))))
     (term-color-cyan ((,class ( :foreground ,cyan))))
     (term-color-white ((,class ( :foreground ,base00))))

     ;; helm
     (helm-selection ((,class (:background ,base02 :underline t))))
     (helm-selection-line ((,class (:background ,base02 :foreground ,base1 :underline nil))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((,class (:foreground unspecified :background ,base02))))
     (helm-swoop-target-line-block-face ((,class (:foreground unspecified :background ,base02))))
     (helm-swoop-target-word-face ((,class (:foreground ,magenta :background unspecified))))

     ;; diff
     (diff-added ((,class (:foreground ,green :background nil))))
     (diff-changed ((,class (:foreground ,blue :background nil))))
     (diff-removed ((,class (:foreground ,red :background nil))))
     (diff-context ((,class (:background nil))))
     (diff-header ((,class (:background ,base2 :foreground ,base01))))
     (diff-file-header ((,class (:background ,base2 :foreground ,base00 :weight bold))))
     (diff-refine-added ((,class (:foreground ,green :background ,base03 :inverse-video t))))
     (diff-refine-change ((,class (:foreground ,blue :background ,base03 :inverse-video t))))
     (diff-refine-removed ((,class (:foreground ,red :background ,base03 :inverse-video t))))

     ;; whitespace
     (trailing-whitespace ((,class (:foreground ,red))))

     (whitespace-empty ((,class (:foreground ,red))))
     (whitespace-hspace ((,class (:foreground ,orange))))
     (whitespace-indentation ((,class (:foreground ,base02))))
     (whitespace-space ((,class (:foreground ,base02))))
     (whitespace-space-after-tab ((,class (:foreground ,cyan))))
     (whitespace-space-before-tab ((,class (:weight bold :foreground ,red))))
     (whitespace-tab ((,class (:foreground ,base02))))
     (whitespace-trailing ((,class (:weight bold :foreground ,red :background ,base02))))
     (whitespace-highlight-face ((,class (:foreground ,red :background ,blue))))
     (whitespace-line ((,class (:foreground ,magenta :background ,base03))))
     )))

(defmacro define-solarized-theme (mode)
  "Define a theme for the solarized variant `MODE'."
  (let ((name (intern (format "solarized-%s" (symbol-name mode))))
        (doc (format "solarized-%s" mode)))
    `(progn
       (deftheme ,name ,doc)
       (with-solarized-colors
        ',mode
        (apply 'custom-theme-set-faces ',name (solarized-face-specs)))
       (provide-theme ',name))))

;;; Extra functions

(defun solarized-mode-name ()
  "Return the mode without the solarized- prefix, e.g. dark or light."
  (intern (substring (symbol-name exordium-theme) 10)))

(defun set-solarized-extra-org-statuses ()
  "Set colors for WORK and WAIT org statuses"
  (with-solarized-colors
   (solarized-mode-name)
   (setq org-todo-keyword-faces
         `(("WORK" . (:background ,yellow :foreground ,back
                      :weight bold :box nil))
           ("WAIT" . (:background ,orange :foreground ,back
                      :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-solarized-dark ()
  "Sets the colors to the solarized dark theme"
  (interactive)
  (with-solarized-colors
   'dark
   (apply 'custom-set-faces (solarized-face-specs))))

(defun set-colors-solarized-light ()
  "Sets the colors to the solarized light theme"
  (interactive)
  (with-solarized-colors
   'light
   (apply 'custom-set-faces (solarized-face-specs))))

(provide 'color-theme-solarized)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; color-theme-solarized.el ends here
