;;; color-theme-tomorrow.el --- A series of 5 pastel color themes.
;;;
;;; Credit:
;;; Inspired by the "Tomorrow" theme from Chris Kempson (MIT license).
;;; See https://github.com/chriskempson/tomorrow-theme

(require 'init-prefs)
(require 'org)

;;; Theme options

(defcustom exordium-tomorrow-modeline-box t
  "Enable displaying a box around the modeline.
Inspired by the apropospriate-theme."
  :group 'exordium
  :type  'boolean)

;;; Color palette.

(defconst tomorrow-colors
  '((night
     . ((background   . "#1d1f21")
        (current-line . "#282a2e")
        (selection    . "#373b41")
        (foreground   . "#c5c8c6")
        (comment      . "#969896")
        (red          . "#cc6666")
        (orange       . "#de935f")
        (yellow       . "#f0c674")
        (green        . "#b5bd68")
        (aqua         . "#8abeb7")
        (blue         . "#81a2be")
        (purple       . "#b294bb")
        (black        . "#000000")))
    (day
     . ((background   . "#ffffff")
        (current-line . "#efefef")
        (selection    . "#d6d6d6")
        (foreground   . "#4d4d4c")
        (comment      . "#8e908c")
        (red          . "#c82829")
        (orange       . "#f5871f")
        (yellow       . "#eab700")
        (green        . "#718c00")
        (aqua         . "#3e999f")
        (blue         . "#4271ae")
        (purple       . "#8959a8")
        (black        . "#000000")))
    (night-eighties
     . ((background   . "#2d2d2d")
        (current-line . "#393939")
        (selection    . "#515151")
        (foreground   . "#cccccc")
        (comment      . "#999999")
        (red          . "#f2777a")
        (orange       . "#f99157")
        (yellow       . "#ffcc66")
        (green        . "#99cc99")
        (aqua         . "#66cccc")
        (blue         . "#6699cc")
        (purple       . "#cc99cc")
        (black        . "#000000")))
    (night-blue
     . ((background   . "#002451")
        (current-line . "#00346e")
        (selection    . "#003f8e")
        (foreground   . "#ffffff")
        (comment      . "#7285b7")
        (red          . "#ff9da4")
        (orange       . "#ffc58f")
        (yellow       . "#ffeead")
        (green        . "#d1f1a9")
        (aqua         . "#99ffff")
        (blue         . "#bbdaff")
        (purple       . "#ebbbff")
        (black        . "#000000")))
    (night-bright
     . ((background   . "#000000")
        (current-line . "#2a2a2a")
        (selection    . "#424242")
        (foreground   . "#eaeaea")
        (comment      . "#969896")
        (red          . "#d54e53")
        (orange       . "#e78c45")
        (yellow       . "#e7c547")
        (green        . "#b9ca4a")
        (aqua         . "#70c0b1")
        (blue         . "#7aa6da")
        (purple       . "#c397d8")
        (black        . "#000000")))))

;;; Theme definition

(defmacro with-tomorrow-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various tomorrow colors.
`MODE' should be set to either 'day, 'night, 'night-eighties,
'night-blue or 'night-bright."
  `(let ((colors (or (cdr (assoc ,mode tomorrow-colors))
                     (error "no such theme flavor"))))
     (let ((background   (cdr (assoc 'background colors)))
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
           (class        '((class color) (min-colors 89))))
       ,@body)))

(defmacro tomorrow-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((t (:foreground ,foreground :background ,background))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:slant italic :weight bold))))
     (underline ((t (:underline t))))
     (italic ((t (:slant italic))))
     (shadow ((t (:background ,black))))
     (success ((t (:foreground ,green))))
     (error ((t (:foreground ,red))))
     (warning ((t (:foreground ,orange))))
     (scroll-bar ((t (:background ,background))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((t (:background ,blue :foreground ,current-line))))
     (show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

     ;; Region
     (region ((t (:background ,selection))))
     (secondary-selection ((t (:background ,current-line))))

     ;; Font-lock
     (font-lock-builtin-face ((t (:foreground ,aqua))))
     (font-lock-comment-delimiter-face ((t (:foreground ,comment :slant italic))))
     (font-lock-comment-face ((t (:foreground ,comment :slant italic))))
     (font-lock-constant-face ((t (:foreground ,aqua))))
     (font-lock-doc-face ((t (:foreground ,comment :slant italic))))
     (font-lock-function-name-face ((t (:foreground ,blue :weight bold))))
     (font-lock-keyword-face ((t (:foreground ,purple :weight bold))))
     (font-lock-negation-char-face ((t (:foreground ,green))))
     (font-lock-preprocessor-face ((t (:foreground ,purple))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,purple))))
     (font-lock-string-face ((t (:foreground ,green))))
     (font-lock-type-face ((t (:foreground ,yellow))))
     (font-lock-variable-name-face ((t (:foreground ,foreground)))) ;orange
     (font-lock-warning-face ((t (:weight bold :foreground ,red))))

     ;; Emacs interface
     (cursor ((t (:background ,red))))
     (fringe ((t (:background ,current-line))))
     ;;(linum ((t (:background ,current-line)))) ; grunge
     (linum ((t (:background ,current-line :foreground ,foreground
                      :height 1.0
                      :inherit nil :weight normal :slant normal :underline nil))))
     (linum-highlight-face ((t (:background ,current-line :foreground ,foreground))))

     (hl-line ((t (:background ,current-line :inherit nil))))
     (highlight ((t (:background ,green :foreground ,background)))) ;+:foreground

     (border ((t (:background ,current-line))))
     (border-glyph ((t (nil))))

     (link ((t (:foreground ,blue :underline t))))
     (link-visited ((t (:foreground ,purple))))

     (gui-element ((t (:background ,current-line :foreground ,foreground))))
     (widget-button ((t (:underline t))))
     (widget-field
      ((t (:background ,current-line :box (:line-width 1 :color ,foreground)))))
     (header-line ((t (:foreground ,purple :background nil))))
     (menu ((t (:foreground ,foreground :background ,selection))))

     ;; Customize
     (custom-variable-tag ((t (:foreground ,blue))))
     (custom-group-tag ((t (:foreground ,blue))))
     (custom-state ((t (:foreground ,green))))

     ;; Mode line
     (mode-line ((t ,(if exordium-tomorrow-modeline-box
                         `(:box (:line-width 4 :color ,black :style nil)
                           :height 0.9
                           :background ,selection :foreground ,foreground)
                       `(:background ,selection :foreground ,foreground)))))
     (mode-line-inactive ((t ,(if exordium-tomorrow-modeline-box
                                  `(:box (:line-width 4 :color ,black :style nil)
                                    :height 0.9
                                    :background ,current-line :foreground ,foreground)
                                `(:background ,current-line :foreground ,foreground)))))
     (mode-line-buffer-id ((t (:foreground ,purple :background nil))))
     (mode-line-emphasis ((t (:foreground ,foreground :slant italic))))
     (mode-line-highlight ((t (:foreground ,purple :box nil :weight bold))))
     (minibuffer-prompt ((t (:foreground ,blue))))
     (which-func ((t (:foreground ,background :weight bold))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,selection))))
     (exordium-powerline-active2 ((t (:background ,current-line))))
     (exordium-powerline-active3 ((t (:background ,purple :foreground ,background))))
     (exordium-powerline-active4 ((t (:background ,red :foreground ,background))))
     (exordium-powerline-active5 ((t (:background ,green :foreground ,background))))
     (exordium-powerline-inactive1 ((t (:background ,current-line :foreground ,comment))))
     (exordium-powerline-inactive2 ((t (:background ,background :foreground ,comment))))
     (exordium-powerline-inactive3 ((t (:background ,comment :foreground ,background))))
     (exordium-project-name ((t (:foreground ,purple))))

     ;; Search
     (match ((t (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((t (:foreground ,yellow :background ,background :inverse-video t))))
     (lazy-highlight
      ((t (:foreground ,aqua :background ,background :inverse-video t))))
     (isearch-fail
      ((t (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,yellow :background ,background :inverse-video t))))
     (hi-pink ((t (:foreground ,purple :background ,background :inverse-video t))))
     (hi-green ((t (:foreground ,green :background ,background :inverse-video t))))
     (hi-blue ((t (:foreground ,aqua :background ,background :inverse-video t))))

     ;; IDO
     (ido-subdir ((t (:foreground ,comment))))
     (ido-first-match ((t (:foreground ,orange :weight bold))))
     (ido-only-match ((t (:foreground ,red :weight bold))))
     (ido-indicator ((t (:foreground ,red :background ,background))))
     (ido-virtual ((t (:foreground ,comment))))

     ;; Helm
     (helm-header ((t (:foreground ,green :background ,background
                       :underline nil :box nil))))
     (helm-source-header ((t (:foreground ,background :background ,purple
                              :underline nil :weight bold :box nil))))
     (helm-selection ((t (:background ,selection :underline nil))))
     (helm-selection-line ((t (:background ,selection))))
     (helm-visible-mark ((t (:foreground ,background :background ,yellow))))
     (helm-candidate-number ((t (:foreground ,green :background ,selection))))
     (helm-swoop-target-line-face ((t (:foreground ,background :background ,yellow))))
     (helm-swoop-target-word-face ((t (:foreground ,background :background ,aqua))))

     ;; Flymake
     (flymake-warnline ((t (:underline ,orange :background ,background))))
     (flymake-errline ((t (:underline ,red :background ,background))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((t (:foreground ,yellow))))
     (compilation-line-number ((t (:foreground ,yellow))))
     (compilation-message-face ((t (:foreground ,blue)))) ; TODO: does not exist
     (compilation-mode-line-exit ((t (:foreground ,green))))
     (compilation-mode-line-fail ((t (:foreground ,red))))
     (compilation-mode-line-run ((t (:foreground ,blue))))

     ;; Rtags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                             `(:background ,red :foreground ,background)
                           `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                              `(:background ,orange :foreground ,background)
                            `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                               `(:background ,green :foreground ,background)
                             `(:underline (:color ,green :style wave))))))
     (rtags-skippedline ((t (:background ,black))))

     ;; Magit
     (magit-branch ((t (:foreground ,green))))
     (magit-header ((t (:inherit nil :weight bold))))
     (magit-item-highlight ((t (:background ,current-line))))
     ;;(magit-item-highlight ((t (:inherit highlight))))
     (magit-item-mark ((t (:inherit region :foreground ,orange))))
     (magit-diff-add ((t (:inherit region :foreground ,green))))
     (magit-diff-del ((t (:inherit region :foreground ,red))))
     (magit-diff-none ((t (:inherit region :foreground ,foreground))))
     (magit-log-graph ((t (:foreground ,comment))))
     (magit-log-sha1 ((t (:foreground ,purple))))
     (magit-log-head-label-bisect-bad ((t (:foreground ,red))))
     (magit-log-head-label-bisect-good ((t (:foreground ,green))))
     (magit-log-head-label-default ((t (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((t (:foreground ,blue))))
     (magit-log-head-label-remote ((t (:foreground ,green))))
     (magit-log-head-label-tags ((t (:foreground ,aqua :box nil :weight bold))))
     (magit-section-title ((t (:inherit diff-hunk-header))))

     ;; Git gutter fringe
     (git-gutter-fr:added ((t (:foreground ,green :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
     (git-gutter-fr:modified ((t (:foreground ,purple :weight bold))))

     ;; Diff
     (diff-added ((t (:foreground ,green))))
     (diff-changed ((t (:foreground ,yellow))))
     (diff-removed ((t (:foreground ,red))))
     (diff-header ((t (:background ,current-line))))
     (diff-file-header ((t (:background ,selection))))
     (diff-hunk-header ((t (:background ,current-line :foreground ,purple))))

     ;; Ediff
     (ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-odd-diff-A ((t (:foreground ,comment :background nil :inverse-video t))))
     (ediff-odd-diff-B ((t (:foreground ,comment :background nil :inverse-video t))))

     ;; Grep
     (grep-context-face ((t (:foreground ,comment))))
     (grep-error-face ((t (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,blue))))
     (grep-match-face ((t (:foreground nil :background nil :inherit match))))

     ;; Org
     (org-level-1 ((t
                    ,(append `(:foreground ,foreground)
                             (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-level-2 ((t (:foreground ,aqua))))
     (org-level-3 ((t (:foreground ,purple))))
     (org-level-4 ((t (:foreground ,comment))))
     (org-agenda-structure ((t (:foreground ,purple))))
     (org-agenda-date ((t (:foreground ,blue :underline nil))))
     (org-agenda-done ((t (:foreground ,green))))
     (org-agenda-dimmed-todo-face ((t (:foreground ,comment))))
     (org-block ((t (:foreground ,orange))))
     (org-code ((t (:foreground ,yellow))))
     (org-column ((t (:background ,current-line))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground ,purple :underline t))))
     (org-document-info ((t (:foreground ,aqua))))
     (org-document-info-keyword ((t (:foreground ,green))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,green)
                                    (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-todo ((t (:foreground ,red :weight bold :box nil))))
     (org-done ((t (:foreground ,green :weight bold :box nil))))
     (org-checkbox ((t (:foreground ,yellow :weight bold))))
     (org-ellipsis ((t (:foreground ,comment))))
     (org-footnote ((t (:foreground ,aqua))))
     (org-formula ((t (:foreground ,red))))
     (org-hide ((t (:foreground ,current-line))))
     (org-link ((t (:foreground ,blue))))
     (org-scheduled ((t (:foreground ,green))))
     (org-scheduled-previously ((t (:foreground ,orange))))
     (org-scheduled-today ((t (:foreground ,green))))
     (org-special-keyword ((t (:foreground ,orange))))
     (org-table ((t (:foreground ,foreground))))
     (org-upcoming-deadline ((t (:foreground ,orange))))
     (org-warning ((t (:weight bold :foreground ,red))))

     ;; Markdown
     (markdown-url-face ((t (:inherit link :foreground ,yellow :weight normal))))
     (markdown-link-face ((t (:foreground ,orange :underline t :weight bold))))
     (markdown-header-face-1 ((t
                               ,(append `(:weight bold :foreground ,blue)
                                        (if exordium-theme-use-big-font '(:height 1.44)) nil))))
     (markdown-header-face-2 ((t
                               ,(append `(:weight bold :foreground ,blue)
                                        (if exordium-theme-use-big-font '(:height 1.2)) nil))))
     (markdown-header-face-3 ((t (:foreground ,blue :weight bold))))
     (markdown-header-face-4 ((t (:foreground ,blue :weight normal))))
     (markdown-header-face-5 ((t (:foreground ,blue :weight bold :slant italic))))
     (markdown-header-delimiter-face ((t (:foreground ,blue))))
     (markdown-bold-face ((t (:foreground ,foreground :weight bold))))
     (markdown-italic-face ((t (:foreground ,foreground :weight normal :slant italic))))
     (markdown-list-face ((t (:foreground ,blue :weight normal))))
     (markdown-inline-code-face ((t (:foreground ,aqua :weight normal))))
     (markdown-markup-face ((t (:foreground ,blue))))
     (markdown-pre-face ((t (:foreground ,aqua))))

     ;; js2-mode
     (js2-warning ((t (:underline (:color ,orange :style wave)))))
     (js2-error ((t (:foreground nil :underline (:color ,red :style wave)))))
     (js2-external-variable ((t (:foreground ,orange))))
     (js2-function-param ((t (:foreground ,blue))))
     (js2-instance-member ((t (:foreground ,blue))))
     (js2-private-function-call ((t (:foreground ,red))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,orange :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,red :style wave)))))

     ;; nxml
     (nxml-name-face
      ((t (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face
      ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face
      ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face
      ((t (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face
      ((t (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((t (:underline ,red))))  ; TODO: does not exist

     ;;; Deft
     (deft-title-face ((t (:foreground ,green :weight bold))))
     (deft-time-face ((t (:foreground ,yellow))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((t (:foreground ,foreground))))
     (undo-tree-visualizer-current-face ((t (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((t (:foreground ,red))))
     (undo-tree-visualizer-register-face ((t (:foreground ,yellow))))

     ;; Trailing whitespaces
     (trailing-whitespace ((t (:background ,red :foreground ,yellow))))
     (whitespace-empty ((t (:foreground ,red :background ,yellow))))
     (whitespace-hspace ((t (:background ,selection :foreground ,comment))))
     (whitespace-indentation ((t (:background ,yellow :foreground ,red))))
     (whitespace-line ((t (:background ,current-line :foreground ,purple))))
     (whitespace-newline ((t (:foreground ,comment))))
     (whitespace-space ((t (:background ,current-line :foreground ,comment))))
     (whitespace-space-after-tab ((t (:background ,yellow :foreground ,red))))
     (whitespace-space-before-tab ((t (:background ,orange :foreground ,red))))
     (whitespace-tab ((t (:background ,selection :foreground ,comment))))
     (whitespace-trailing ((t (:background ,red :foreground ,yellow))))
     (hl-sexp-face ((t (:background ,current-line))))
     (highlight-80+ ((t (:background ,current-line))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match
      ((t (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch
      ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match
      ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; TODO: Lisp programming faces
     (eval-sexp-fu-flash ((t (:background ,orange :foreground ,background))))
     (eval-sexp-fu-flash-error ((t (:background ,red :foreground ,background))))

     ;; Clojure
     (clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
     (clojure-test-success-face
      ((t (:background nil :foreground nil :underline ,green))))
     (clojure-keyword ((t (:foreground ,yellow))))
     (clojure-parens ((t (:foreground ,foreground))))
     (clojure-braces ((t (:foreground ,green))))
     (clojure-brackets ((t (:foreground ,yellow))))
     (clojure-double-quote ((t (:foreground ,aqua :background nil))))
     (clojure-special ((t (:foreground ,blue))))
     (clojure-java-call ((t (:foreground ,purple))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,foreground))))
     (rainbow-delimiters-unmatched-face ((t (:foreground ,red))))

     ;; Slime
     (sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((t (:weight bold))))
     (slime-repl-input-face ((t (:weight normal :underline nil))))
     (slime-repl-prompt-face ((t (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((t (:foreground ,green))))
     (slime-repl-output-face ((t (:foreground ,blue :background ,background))))

     ;; auctex
     (font-latex-bold-face ((t (:foreground ,green))))
     (font-latex-doctex-documentation-face ((t (:background ,current-line))))
     (font-latex-italic-face ((t (:foreground ,green))))
     (font-latex-math-face ((t (:foreground ,orange))))
     (font-latex-sectioning-0-face ((t (:foreground ,yellow))))
     (font-latex-sectioning-1-face ((t (:foreground ,yellow))))
     (font-latex-sectioning-2-face ((t (:foreground ,yellow))))
     (font-latex-sectioning-3-face ((t (:foreground ,yellow))))
     (font-latex-sectioning-4-face ((t (:foreground ,yellow))))
     (font-latex-sectioning-5-face ((t (:foreground ,yellow))))
     (font-latex-sedate-face ((t (:foreground ,aqua))))
     (font-latex-string-face ((t (:foreground ,yellow))))
     (font-latex-verbatim-face ((t (:foreground ,orange))))
     (font-latex-warning-face ((t (:foreground ,red))))

     ;; Others
     (which-func ((t (:foreground ,blue :background nil :weight bold))))

     (csv-separator-face ((t (:foreground ,orange))))
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

;;; Extra functions

(defun tomorrow-mode-name ()
  "Return the mode without the tomorrow- prefix, e.g. day, night etc."
  (intern (substring (symbol-name exordium-theme) 9)))

(defun set-tomorrow-extra-org-statuses ()
  (with-tomorrow-colors
   (tomorrow-mode-name)
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,yellow
                      :foreground ,yellow
                      :weight bold :box nil))
           ("WAIT" . (;:background ,orange
                      :foreground ,orange
                      :weight bold :box nil))))))

;; Debugging functions

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

(provide 'color-theme-tomorrow)

;;; color-theme-tomorrow.el ends here
