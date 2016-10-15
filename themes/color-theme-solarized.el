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
     (default ((t (:foreground ,base0 :background ,back))))
     (shadow ((t (:background ,base01))))
     (success ((t (:foreground ,green))))
     (error ((t (:weight bold :foreground ,red))))
     (warning ((t (:weight bold :foreground ,orange))))
     (scroll-bar ((t (:background ,back))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((t (:weight bold :foreground ,cyan :background ,base02))))
     (show-paren-mismatch ((t (:weight bold :foreground ,red :background ,base01))))

     ;; Region
     (region ((t (:background ,base00))))
     (secondary-selection ((t (:background ,base02))))

     ;; font-lock
     (font-lock-builtin-face ((t (:foreground ,green :slant italic))))
     (font-lock-comment-face ((t (:foreground ,base01 :slant italic))))
     (font-lock-constant-face ((t (:foreground ,cyan))))
     (font-lock-function-name-face ((t (:foreground ,blue))))
     (font-lock-keyword-face ((t (:foreground ,green))))
     (font-lock-string-face ((t (:foreground ,cyan))))
     (font-lock-type-face ((t (:foreground ,yellow))))
     (font-lock-variable-name-face ((t (:foreground ,blue))))
     (font-lock-warning-face ((t (:foreground ,red :weight bold))))
     (font-lock-doc-face ((t (:foreground ,base01 :slant italic))))
     (font-lock-color-constant-face ((t (:foreground ,green))))
     (font-lock-comment-delimiter-face ((t (:foreground ,base01 :slant italic))))
     (font-lock-preprocessor-face ((t (:foreground ,orange))))
     (font-lock-reference-face ((t (:foreground ,cyan))))
     (font-lock-negation-char-face ((t (:foreground ,red))))
     (font-lock-other-type-face ((t (:foreground ,blue :slant italic))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,orange))))
     (font-lock-special-keyword-face ((t (:foreground ,red))))
     (font-lock-exit-face ((t (:foreground ,red))))
     (font-lock-other-emphasized-face ((t (:foreground ,violet))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))

     ;; Emacs interface
     (cursor ((t (:foreground ,base03 :background ,base0))))
     (escape-glyph-face ((t (:foreground ,red))))
     (fringe ((t (:foreground ,base01 :background ,base02))))
     (linum ((t (:foreground ,base01 :background ,base02))))
     (header-line ((t (:foreground ,base0 :background ,base02 :weight bold))))
     (highlight ((t (:background ,base02))))
     (hl-line ((t (:background ,base02))))
     (menu ((t (:foreground ,base0 :background ,base02))))
     (link ((t (:foreground ,violet :underline t))))
     (link-visited ((t (:foreground ,magenta :underline t))))
     (vertical-border ((t (:foreground ,base0))))

     ;; Mode line
     (minibuffer-prompt ((t (:weight bold :foreground ,cyan))))
     (mode-line ((t (:foreground ,base1 :background ,base01 :box nil))))
     (mode-line-inactive ((t (:foreground ,base00 :background ,base02 :box nil))))
     (which-func ((t (:foreground ,back :weight bold))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,base01 :foreground ,back))))
     (exordium-powerline-active2 ((t (:background ,base02 :foreground ,base01))))
     (exordium-powerline-active3 ((t (:background ,base2 :foreground ,back))))
     (exordium-powerline-active4 ((t (:background ,red :foreground ,back))))
     (exordium-powerline-active5 ((t (:background ,green :foreground ,back))))
     (exordium-powerline-inactive1 ((t (:background ,base02))))
     (exordium-powerline-inactive2 ((t (:background ,base02))))
     (exordium-powerline-inactive3 ((t (:background ,base00 :foreground ,back))))
     (exordium-project-name ((t (:background ,violet :foreground ,back))))

     ;; Search
     (isearch ((t (:foreground ,orange :background ,back))))
     (isearch-fail ((t (:foreground ,orange :background ,back))))
     (lazy-highlight ((t (:foreground ,yellow :background ,back))))
     (match ((t (:foreground ,yellow :background ,back))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,yellow :background ,back :inverse-video t))))
     (hi-pink ((t (:foreground ,magenta :background ,back :inverse-video t))))
     (hi-green ((t (:foreground ,green :background ,back :inverse-video t))))
     (hi-blue ((t (:foreground ,cyan :background ,back :inverse-video t))))

     ;; Compilation
     (compilation-info ((t (:weight bold :foreground ,green))))
     (compilation-warning ((t (:weight bold :foreground ,orange))))

     ;; Custom
     (custom-button
      ((t (:foreground ,base1 :background ,base02
           :box (:line-width 2 :style released-button)))))
     (custom-button-mouse
      ((t (:foreground ,base1 :background ,base02 :inherit custom-button))))
     (custom-button-pressed
      ((t (:foreground ,base1 :background ,base02
           :box (:line-width 2 :style pressed-button)
           :inherit custom-button-mouse))))
     (custom-changed ((t (:foreground ,blue :background ,base3))))
     (custom-comment ((t (:foreground ,base1 :background ,base02))))
     (custom-comment-tag ((t (:foreground ,base1 :background ,base02))))
     (custom-documentation ((t (:inherit default))))
     (custom-group-tag ((t (:foreground ,base1))))
     (custom-group-tag-1 ((t (:weight bold :foreground ,base1))))
     (custom-invalid ((t (:foreground ,red :background ,back))))
     (custom-link ((t (:foreground ,violet))))
     (custom-state ((t (:foreground ,green))))
     (custom-variable-tag ((t (:foreground ,base1))))

     ;; Info
     (info-xref ((t (:foreground ,blue))))
     (info-xref-visited ((t (:foreground ,magenta :inherit info-xref))))

     ;; RTags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,red :foreground ,back)
                                `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,orange :foreground ,back)
                                 `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,green :foreground ,back)
                                  `(:underline (:color ,green :style wave))))))
     (rtags-skippedline ((t (:background ,base01))))

     ;; Org
     (org-hide ((t (:foreground ,base03))))
     (org-todo ((t (:weight bold :foreground ,base03 :background ,red))))
     (org-done ((t (:weight bold :foreground ,green))))
     (org-todo-kwd-face ((t (:foreground ,red :background ,base03))))
     (org-done-kwd-face ((t (:foreground ,green :background ,base03))))
     (org-project-kwd-face ((t (:foreground ,violet :background ,base03))))
     (org-waiting-kwd-face ((t (:foreground ,orange :background ,base03))))
     (org-someday-kwd-face ((t (:foreground ,blue :background ,base03))))
     (org-started-kwd-face ((t (:foreground ,yellow :background ,base03))))
     (org-cancelled-kwd-face ((t (:foreground ,green :background ,base03))))
     (org-delegated-kwd-face ((t (:foreground ,cyan :background ,base03))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,cyan)
                                    (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-level-1 ((t
                    ,(append `(:foreground ,base0)
                             (if exordium-theme-use-big-font '(:height 1.44) nil)))))

     ;; outline
     (outline-1 ((t (:foreground ,blue))))
     (outline-2 ((t (:foreground ,cyan))))
     (outline-3 ((t (:foreground ,yellow))))
     (outline-4 ((t (:foreground ,red))))
     (outline-5 ((t (:foreground ,base0))))
     (outline-6 ((t (:foreground ,base01))))
     (outline-7 ((t (:foreground ,orange))))
     (outline-8 ((t (:foreground ,violet))))

     ;; Flymake
     (flymake-errline ((t (:weight bold :foreground ,red :background ,back))))
     (flymake-warnline ((t (:weight bold :foreground ,orange :background ,back))))

     ;; js2-mode
     (js2-warning ((t (:underline ,orange :style wave))))
     (js2-error ((t (:foreground nil :underline ,red :style wave))))
     (js2-external-variable ((t (:foreground ,violet))))
     (js2-function-param ((t (:foreground ,blue))))
     (js2-instance-member ((t (:foreground ,blue))))
     (js2-private-function-call ((t (:foreground ,red))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,orange :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,red :style wave)))))

     ;; git-gutter
     (git-gutter:modified ((t (:foreground ,violet))))
     (git-gutter:added ((t (:foreground ,green))))
     (git-gutter:deleted ((t (:foreground ,red))))

     ;; git-gutter-fringe
     (git-gutter-fr:added ((t (:foreground ,green :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
     (git-gutter-fr:modified ((t (:foreground ,violet :weight bold))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,red))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,blue))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,orange))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,magenta))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,base0))))

     ;; slime
     (slime-error-face ((t (:foreground ,red))))
     (slime-note-face ((t (:foreground ,yellow))))
     (slime-repl-inputted-output-face ((t (:foreground ,red))))
     (slime-repl-output-mouseover-face ((t (:box (:color ,base3)))))
     (slime-style-warning-face ((t (:weight bold :foreground ,orange))))
     (slime-warning-face ((t (:weight bold :foreground ,red))))

     ;;flyspell
     (flyspell-incorrect ((t (:foreground ,red))))
     (flyspell-duplicate ((t (:foreground ,yellow))))

     ;;ansi-term
     (term-color-black ((t ( :foreground ,base02))))
     (term-color-red ((t ( :foreground ,red))))
     (term-color-green ((t ( :foreground ,green))))
     (term-color-yellow ((t ( :foreground ,yellow))))
     (term-color-blue ((t ( :foreground ,blue))))
     (term-color-magenta ((t ( :foreground ,magenta))))
     (term-color-cyan ((t ( :foreground ,cyan))))
     (term-color-white ((t ( :foreground ,base00))))

     ;; helm
     (helm-selection ((t (:background ,base02 :underline t))))
     (helm-selection-line ((t (:background ,base02 :foreground ,base1 :underline nil))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((t (:foreground unspecified :background ,base02))))
     (helm-swoop-target-line-block-face ((t (:foreground unspecified :background ,base02))))
     (helm-swoop-target-word-face ((t (:foreground ,magenta :background unspecified))))

     ;; diff
     (diff-added ((t (:foreground ,green :background nil))))
     (diff-changed ((t (:foreground ,blue :background nil))))
     (diff-removed ((t (:foreground ,red :background nil))))
     (diff-context ((t (:background nil))))
     (diff-header ((t (:background ,base2 :foreground ,base01))))
     (diff-file-header ((t (:background ,base2 :foreground ,base00 :weight bold))))
     (diff-refine-added ((t (:foreground ,green :background ,base03 :inverse-video t))))
     (diff-refine-change ((t (:foreground ,blue :background ,base03 :inverse-video t))))
     (diff-refine-removed ((t (:foreground ,red :background ,base03 :inverse-video t))))

     ;; whitespace
     (trailing-whitespace ((t (:foreground ,red))))

     (whitespace-empty ((t (:foreground ,red))))
     (whitespace-hspace ((t (:foreground ,orange))))
     (whitespace-indentation ((t (:foreground ,base02))))
     (whitespace-space ((t (:foreground ,base02))))
     (whitespace-space-after-tab ((t (:foreground ,cyan))))
     (whitespace-space-before-tab ((t (:weight bold :foreground ,red))))
     (whitespace-tab ((t (:foreground ,base02))))
     (whitespace-trailing ((t (:weight bold :foreground ,red :background ,base02))))
     (whitespace-highlight-face ((t (:foreground ,red :background ,blue))))
     (whitespace-line ((t (:foreground ,magenta :background ,base03))))

     ;; Emacs Lisp
     (eval-sexp-fu-flash ((t (:background ,orange :foreground ,back))))
     (eval-sexp-fu-flash-error ((t (:background ,red :foreground ,back))))
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
