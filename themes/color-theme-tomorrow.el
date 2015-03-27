;;;; Inspired by the "Tomorrow" theme from Chris Kempson (MIT license)

(defconst tomorrow-colors
  '((night . ((background . "#1d1f21")
              (current-line . "#282a2e")
              (selection . "#373b41")
              (foreground . "#c5c8c6")
              (comment . "#969896")
              (red . "#cc6666")
              (orange . "#de935f")
              (yellow . "#f0c674")
              (green . "#b5bd68")
              (aqua . "#8abeb7")
              (blue . "#81a2be")
              (purple . "#b294bb")
              (black . "#000000")))
    (day . ((background . "#ffffff")
            (current-line . "#efefef")
            (selection . "#d6d6d6")
            (foreground . "#4d4d4c")
            (comment . "#8e908c")
            (red . "#c82829")
            (orange . "#f5871f")
            (yellow . "#eab700")
            (green . "#718c00")
            (aqua . "#3e999f")
            (blue . "#4271ae")
            (purple . "#8959a8")
            (black . "#000000")))
    (night-eighties . ((background . "#2d2d2d")
                       (current-line . "#393939")
                       (selection . "#515151")
                       (foreground . "#cccccc")
                       (comment . "#999999")
                       (red . "#f2777a")
                       (orange . "#f99157")
                       (yellow . "#ffcc66")
                       (green . "#99cc99")
                       (aqua . "#66cccc")
                       (blue . "#6699cc")
                       (purple . "#cc99cc")
                       (black . "#000000")))
    (night-blue . ((background . "#002451")
                   (current-line . "#00346e")
                   (selection . "#003f8e")
                   (foreground . "#ffffff")
                   (comment . "#7285b7")
                   (red . "#ff9da4")
                   (orange . "#ffc58f")
                   (yellow . "#ffeead")
                   (green . "#d1f1a9")
                   (aqua . "#99ffff")
                   (blue . "#bbdaff")
                   (purple . "#ebbbff")
                   (black . "#000000")))
    (night-bright . ((background . "#000000")
                     (current-line . "#2a2a2a")
                     (selection . "#424242")
                     (foreground . "#eaeaea")
                     (comment . "#969896")
                     (red . "#d54e53")
                     (orange . "#e78c45")
                     (yellow . "#e7c547")
                     (green . "#b9ca4a")
                     (aqua . "#70c0b1")
                     (blue . "#7aa6da")
                     (purple . "#c397d8")
                     (black . "#000000")))))

(defmacro with-tomorrow-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various tomorrow colors.
`MODE' should be set to either 'day, 'night, 'night-eighties,
'night-blue or 'night-bright."
  `(let* ((colors (or (cdr (assoc ,mode tomorrow-colors))
                      (error "no such theme flavor")))
          (background   (cdr (assoc 'background colors)))
          (current-line (cdr (assoc 'current-line colors)))
          (selection    (cdr (assoc 'selection colors)))
          (foreground   (cdr (assoc 'foreground colors)))
          (comment      (cdr (assoc 'comment colors)))
          (red          (cdr (assoc 'red colors)))
          (orange       (cdr (assoc 'orange colors)))
          (yellow       (cdr (assoc 'yellow colors)))
          (green        (cdr (assoc 'green colors)))
          (aqua         (cdr (assoc 'aqua colors)))
          (blue         (cdr (assoc 'blue colors)))
          (purple       (cdr (assoc 'purple colors)))
          (black        (cdr (assoc 'black colors)))
          (class '((class color) (min-colors 89))))
     ,@body))

(defmacro tomorrow-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((,class (:foreground ,foreground :background ,background))))
     (bold ((,class (:weight bold))))
     (bold-italic ((,class (:slant italic :weight bold))))
     (underline ((,class (:underline t))))
     (italic ((,class (:slant italic))))
     (shadow ((,class (:foreground ,comment))))
     (success ((,class (:foreground ,green))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange))))
     (outline-4 ((,class (:slant normal :foreground ,comment))))

     ;; Font-lock
     (font-lock-builtin-face ((,class (:foreground ,aqua))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,aqua))))
     (font-lock-doc-face ((,class (:foreground ,comment :slant italic))))
     (font-lock-doc-string-face ((,class (:foreground ,yellow))))
     (font-lock-function-name-face ((,class (:foreground ,blue :weight bold))))
     (font-lock-keyword-face ((,class (:foreground ,purple :weight bold))))
     (font-lock-negation-char-face ((,class (:foreground ,green))))
     (font-lock-preprocessor-face ((,class (:foreground ,purple))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
     (font-lock-string-face ((,class (:foreground ,green))))
     (font-lock-type-face ((,class (:foreground ,yellow))))
     (font-lock-variable-name-face ((,class (:foreground ,foreground)))) ;orange
     (font-lock-warning-face ((,class (:weight bold :foreground ,red))))

     ;; Emacs interface
     (cursor ((,class (:background ,red))))
     (fringe ((,class (:background ,current-line))))
     ;;(linum ((,class (:background ,current-line)))) ; grunge
     (linum ((,class (:background ,current-line :foreground ,foreground
                      :height 1.0
                      :inherit nil :weight normal :slant normal :underline nil))))
     (hl-line ((,class (:background ,current-line :inherit nil))))
     (border ((,class (:background ,current-line))))
     (border-glyph ((,class (nil))))
     (highlight ((,class (:background ,green :foreground ,background)))) ;+:foreground
     (link ((,class (:foreground ,blue :underline t))))
     (link-visited ((,class (:foreground ,purple))))
     (gui-element ((,class (:background ,current-line :foreground ,foreground))))

     (mode-line ((,class (:background ,selection :foreground ,foreground))))
     (mode-line-inactive ((,class (:background ,current-line :foreground ,foreground))))
     (mode-line-buffer-id ((,class (:foreground ,purple :background nil))))
     (mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
     (mode-line-highlight ((,class (:foreground ,purple :box nil :weight bold))))
     (minibuffer-prompt ((,class (:foreground ,blue))))

     (region ((,class (:background ,selection))))
     (secondary-selection ((,class (:background ,current-line))))

     (widget-button ((,class (:underline t))))
     (widget-field
      ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

     (header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))
     (menu ((,class (:foreground ,foreground :background ,selection))))

     (custom-variable-tag ((,class (:foreground ,blue))))
     (custom-group-tag ((,class (:foreground ,blue))))
     (custom-state ((,class (:foreground ,green))))

     ;; Search
     (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
     (isearch-lazy-highlight-face
      ((,class (:foreground ,aqua :background ,background :inverse-video t))))
     (isearch-fail
      ((,class
        (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,comment))))
     (ido-first-match ((,class (:foreground ,orange :weight bold))))
     (ido-only-match ((,class (:foreground ,red :weight bold))))
     (ido-indicator ((,class (:foreground ,red :background ,background))))
     (ido-virtual ((,class (:foreground ,comment))))

     ;; Helm
     (helm-header ((,class (:foreground ,green
                            :background ,background
                            :underline nil
                            :box nil))))
     (helm-source-header ((,class (:foreground ,background
                                   :background ,purple
                                   :underline nil
                                   :weight bold
                                   :box nil))))
     (helm-selection ((,class (:background ,selection :underline nil))))
     (helm-selection-line ((,class (:background ,selection))))
     (helm-visible-mark ((,class (:foreground ,background :background ,yellow))))
     (helm-candidate-number ((,class (:foreground ,green :background ,selection))))
     (helm-swoop-target-line-face ((,class (:foreground ,background :background ,yellow))))
     (helm-swoop-target-word-face ((,class (:foreground ,background :background ,aqua))))

     ;; which-func-mode
     (which-func ((,class (:foreground ,blue :background nil :weight bold))))

     ;; Trailing whitespaces
     (trailing-whitespace ((,class (:background ,red :foreground ,yellow))))
     (whitespace-empty ((,class (:foreground ,red :background ,yellow))))
     (whitespace-hspace ((,class (:background ,selection :foreground ,comment))))
     (whitespace-indentation ((,class (:background ,yellow :foreground ,red))))
     (whitespace-line ((,class (:background ,current-line :foreground ,purple))))
     (whitespace-newline ((,class (:foreground ,comment))))
     (whitespace-space ((,class (:background ,current-line :foreground ,comment))))
     (whitespace-space-after-tab ((,class (:background ,yellow :foreground ,red))))
     (whitespace-space-before-tab ((,class (:background ,orange :foreground ,red))))
     (whitespace-tab ((,class (:background ,selection :foreground ,comment))))
     (whitespace-trailing ((,class (:background ,red :foreground ,yellow))))
     (hl-sexp-face ((,class (:background ,current-line))))
     (highlight-80+ ((,class (:background ,current-line))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:background ,blue :foreground ,current-line))))
     (show-paren-mismatch ((,class (:background ,orange :foreground ,current-line))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match
      ((,class (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch
      ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match
      ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; Parenthesis dimming (parenface)
     (paren-face ((,class (:foreground ,comment :background nil))))

     ;; Flymake
     (flymake-warnline ((,class (:underline ,orange :background ,background))))
     (flymake-errline ((,class (:underline ,red :background ,background))))

     ;; Rtags
     ;; Loud:
     ;;(rtags-errline ((,class (:background ,red :foreground ,background))))
     ;;(rtags-warnline ((,class (:background ,orange :foreground ,background))))
     ;;(rtags-fixitline ((,class (:background ,green :foreground ,background))))
     (rtags-errline ((,class (:underline (:color ,red :style wave)))))
     (rtags-warnline ((,class (:underline (:color ,orange :style wave)))))
     (rtags-fixitline ((,class (:underline (:color ,green :style wave)))))

     ;;; Deft
     (deft-title-face ((,class (:foreground ,green :weight bold))))
     (deft-time-face ((,class (:foreground ,yellow))))

     ;; Clojure errors
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face
      ((,class (:background nil :foreground nil :underline ,green))))

     ;; For Brian Carper's extended clojure syntax table
     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,foreground))))
     (clojure-braces ((,class (:foreground ,green))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,aqua :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,purple))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,purple))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,aqua))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,red))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,comment))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; Slime
     (sh-heredoc
      ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:weight bold))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((,class (:foreground ,green))))
     (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     ;; Csv
     (csv-separator-face ((,class (:foreground ,orange))))

     ;; Diff
     (diff-added ((,class (:foreground ,green))))
     (diff-changed ((,class (:foreground ,yellow))))
     (diff-removed ((,class (:foreground ,red))))
     (diff-header ((,class (:background ,current-line))))
     (diff-file-header ((,class (:background ,selection))))
     (diff-hunk-header ((,class (:background ,current-line :foreground ,purple))))

     (ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
     (ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
     (ediff-odd-diff-A
      ((,class (:foreground ,comment :background nil :inverse-video t))))
     (ediff-odd-diff-B
      ((,class (:foreground ,comment :background nil :inverse-video t))))

     (eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; auctex
     (font-latex-bold-face ((,class (:foreground ,green))))
     (font-latex-doctex-documentation-face ((,class (:background ,current-line))))
     (font-latex-italic-face ((,class (:foreground ,green))))
     (font-latex-math-face ((,class (:foreground ,orange))))
     (font-latex-sectioning-0-face ((,class (:foreground ,yellow))))
     (font-latex-sectioning-1-face ((,class (:foreground ,yellow))))
     (font-latex-sectioning-2-face ((,class (:foreground ,yellow))))
     (font-latex-sectioning-3-face ((,class (:foreground ,yellow))))
     (font-latex-sectioning-4-face ((,class (:foreground ,yellow))))
     (font-latex-sectioning-5-face ((,class (:foreground ,yellow))))
     (font-latex-sedate-face ((,class (:foreground ,aqua))))
     (font-latex-string-face ((,class (:foreground ,yellow))))
     (font-latex-verbatim-face ((,class (:foreground ,orange))))
     (font-latex-warning-face ((,class (:foreground ,red))))

     ;; dired+
     (diredp-compressed-file-suffix ((,class (:foreground ,blue))))
     (diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))))
     (diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
     (diredp-exec-priv ((,class (:foreground ,blue :background nil))))
     (diredp-executable-tag ((,class (:foreground ,red :background nil))))
     (diredp-file-name ((,class (:foreground ,yellow))))
     (diredp-file-suffix ((,class (:foreground ,green))))
     (diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     (diredp-ignored-file-name ((,class (:foreground ,comment))))
     (diredp-link-priv ((,class (:background nil :foreground ,purple))))
     (diredp-mode-line-flagged ((,class (:foreground ,red))))
     (diredp-mode-line-marked ((,class (:foreground ,green))))
     (diredp-no-priv ((,class (:background nil))))
     (diredp-number ((,class (:foreground ,yellow))))
     (diredp-other-priv ((,class (:background nil :foreground ,purple))))
     (diredp-rare-priv ((,class (:foreground ,red :background nil))))
     (diredp-read-priv ((,class (:foreground ,green :background nil))))
     (diredp-symlink ((,class (:foreground ,purple))))
     (diredp-write-priv ((,class (:foreground ,yellow :background nil))))

     ;; Magit
     (magit-branch ((,class (:foreground ,green))))
     (magit-header ((,class (:inherit nil :weight bold))))
     (magit-item-highlight ((,class (:background ,current-line))))
     ;;(magit-item-highlight ((,class (:inherit highlight))))
     (magit-item-mark ((, class (:inherit region :foreground ,orange))))
     (magit-diff-add ((,class (:inherit region :foreground ,green))))
     (magit-diff-del ((,class (:inherit region :foreground ,red))))
     (magit-diff-none ((,class (:inherit region :foreground ,foreground))))
     (magit-log-graph ((,class (:foreground ,comment))))
     (magit-log-sha1 ((,class (:foreground ,purple))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,blue))))
     (magit-log-head-label-remote ((,class (:foreground ,green))))
     (magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
     (magit-section-title ((,class (:inherit diff-hunk-header))))

     ;; Git gutter fringe
     ;; Note: this uses linum color, but green/red/yellow are another option.
     (git-gutter-fr:added ((,class (:foreground ,comment))))
     (git-gutter-fr:deleted ((,class (:foreground ,comment))))
     (git-gutter-fr:modified ((,class (:foreground ,comment))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue))))
     (compilation-mode-line-exit ((,class (:foreground ,green))))
     (compilation-mode-line-fail ((,class (:foreground ,red))))
     (compilation-mode-line-run ((,class (:foreground ,blue))))

     ;; Grep
     (grep-context-face ((,class (:foreground ,comment))))
     (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     (regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

     ;; mark-multiple
     (mm/master-face ((,class (:inherit region :foreground nil :background nil))))
     (mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

     ;; Org
     (org-level-1 ((,class (:foreground ,foreground :height 1.44))))
     (org-level-2 ((,class (:foreground ,aqua))))
     (org-level-3 ((,class (:foreground ,purple))))
     (org-level-4 ((,class (:foreground ,comment))))
     (org-agenda-structure ((,class (:foreground ,purple))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-done ((,class (:foreground ,green))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     (org-block ((,class (:foreground ,orange))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,current-line))))
     (org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     (org-date ((,class (:foreground ,purple :underline t))))
     (org-document-info ((,class (:foreground ,aqua))))
     (org-document-info-keyword ((,class (:foreground ,green))))
     (org-document-title ((,class (:weight bold :foreground ,green :height 1.44))))
     (org-todo ((,class (:foreground ,red :weight bold :box nil))))
     (org-done ((,class (:foreground ,green :weight bold :box nil))))
     (org-checkbox ((,class (:foreground ,yellow :weight bold))))
     (org-ellipsis ((,class (:foreground ,comment))))
     (org-footnote ((,class (:foreground ,aqua))))
     (org-formula ((,class (:foreground ,red))))
     (org-hide ((,class (:foreground ,current-line))))
     (org-link ((,class (:foreground ,blue))))
     (org-scheduled ((,class (:foreground ,green))))
     (org-scheduled-previously ((,class (:foreground ,orange))))
     (org-scheduled-today ((,class (:foreground ,green))))
     (org-special-keyword ((,class (:foreground ,orange))))
     (org-table ((,class (:foreground ,foreground))))
     (org-upcoming-deadline ((,class (:foreground ,orange))))
     (org-warning ((,class (:weight bold :foreground ,red))))

     ;; Markdown
     (markdown-url-face ((,class (:inherit link))))
     (markdown-link-face ((,class (:foreground ,blue :underline t))))
     ;;(markdown-header-face-1 ((, class (:foreground ,blue :height 1.44))))

     ;; Fic-mode (highlight FIXME TODO etc) - Note: disabled for now
     (font-lock-fic-face ((, class (:foreground ,red :weight bold :slant italic))))

     ;; Python-specific overrides
     (py-builtins-face ((,class (:foreground ,orange :weight normal))))

     ;; js2-mode
     (js2-warning ((,class (:underline (:color ,orange :style wave)))))
     (js2-error ((,class (:foreground nil :underline (:color ,red :style wave)))))
     (js2-external-variable ((,class (:foreground ,orange))))
     (js2-function-param ((,class (:foreground ,blue))))
     (js2-instance-member ((,class (:foreground ,blue))))
     (js2-private-function-call ((,class (:foreground ,red))))

     ;; nxml
     (nxml-name-face
      ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face
      ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face
      ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face
      ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face
      ((,class (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((,class (:underline ,red))))
     )))

(defmacro define-tomorrow-theme (mode)
  "Define a theme for the tomorrow variant `MODE'."
  (let ((name (intern (format "tomorrow-%s" (symbol-name mode))))
        (doc (format "tomorrow-%s" mode)))
    `(progn
       (deftheme ,name ,doc)
       (with-tomorrow-colors
        ',mode
        (apply 'custom-theme-set-faces ',name (tomorrow-face-specs)))
       (provide-theme ',name))))

(defun set-colors-tomorrow-day ()
  "Sets the colors to the tomorrow day theme"
  (interactive)
  (with-tomorrow-colors
   'day
   (apply 'custom-set-faces (tomorrow-face-specs))))

(defun set-colors-tomorrow-night ()
  "Sets the colors to the tomorrow night theme"
  (interactive)
  (with-tomorrow-colors
   'night
   (apply 'custom-set-faces (tomorrow-face-specs))))

(defun set-colors-tomorrow-night-bright ()
  "Sets the colors to the tomorrow night bright theme"
  (interactive)
  (with-tomorrow-colors
   'night-bright
   (apply 'custom-set-faces (tomorrow-face-specs))))

(defun set-colors-tomorrow-night-eighties ()
  "Sets the colors to the tomorrow night night 80's theme"
  (interactive)
  (with-tomorrow-colors
   'night-eighties
   (apply 'custom-set-faces (tomorrow-face-specs))))

(defun set-colors-tomorrow-night-blue ()
  "Sets the colors to the tomorrow night blue theme"
  (interactive)
  (with-tomorrow-colors
   'night-blue
   (apply 'custom-set-faces (tomorrow-face-specs))))

(defun tomorrow-mode-name ()
  "Return the mode without the tomorrow- prefix, e.g. day, night
etc."
  (intern (substring (symbol-name exordium-theme) 9)))

(defun set-tomorrow-extra-org-statuses ()
  (require 'org)
  (with-tomorrow-colors
   (tomorrow-mode-name)
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,yellow
                      :foreground ,yellow
                      :weight bold :box nil))
           ("WAIT" . (;:background ,orange
                      :foreground ,orange
                      :weight bold :box nil))))))

(provide 'color-theme-tomorrow)
