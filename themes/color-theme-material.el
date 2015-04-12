;;; color-theme-material.el --- based on the colors of the Google Material Design
;;;
;;; Credit:
;;; Inspired by the theme from Paulik Christoph
;;; See https://github.com/cpaulik/emacs-material-theme
;;;
;;; FIXME: replace all custom colors in spec with constants.

(require 'org)

;;; Color palette

(defconst material-colors
  '((background          . "#263238")
    (current-line        . "#37474f")
    (far-background      . "#1c1f26")
    (subtle              . "#a7adba")
    (selection           . "#555555")
    (secondary-selection . "#bf616a")
    (foreground          . "#ffffff")
    (comment             . "#b0bec5")
    (red                 . "#f36c60")
    (orange              . "#ff9800")
    (yellow              . "#fff59d")
    (green               . "#8bc34a")
    (aqua                . "#81d4fa")
    (blue                . "#4dd0e1")
    (purple              . "#b39ddb")
    (black               . "#000000")))

;;; Theme definition

(defmacro with-material-colors (&rest body)
  "Execute `BODY' in a scope with variables bound to the material colors."
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   material-colors))
     ,@body))

(defmacro material-face-specs ()
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
     (success ((,class (:foreground "SeaGreen2"))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange))))

     ;; Parenthesis matching (built-in)
     (show-paren-match-face ((,class (:background "dodgerblue1" :foreground "white"))))
     (show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

     ;; Region
     (region ((,class (:background ,selection :foreground ,background))))
     (secondary-selection ((,class (:background ,secondary-selection))))

     ;; Font lock
     (font-lock-builtin-face ((,class (:foreground "#ff7043"))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
     (font-lock-comment-face ((,class (:foreground ,comment))))
     (font-lock-constant-face ((,class (:foreground ,green))))
     (font-lock-doc-face ((,class (:foreground "moccasin"))))
     (font-lock-doc-string-face ((,class (:foreground ,yellow))))
     (font-lock-function-name-face ((,class (:foreground ,"#b3e5fc"))))
     (font-lock-keyword-face ((,class (:foreground ,yellow))))
     (font-lock-negation-char-face ((,class (:foreground ,blue))))
     (font-lock-preprocessor-face ((,class (:foreground "gold"))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
     (font-lock-string-face ((,class (:foreground "#9ccc65"))))
     (font-lock-type-face ((,class (:foreground "CadetBlue1"))))
     (font-lock-variable-name-face ((,class (:foreground ,yellow))))
     (font-lock-warning-face ((,class (:weight bold :foreground ,red))))

     ;; Emacs interface
     (cursor ((,class (:background ,orange))))
     (fringe ((,class (:background ,background))))

     (linum ((,class (:background ,background :foreground ,subtle
                      :underline nil :weight normal :box nil :slant normal))))
     (linum-highlight-face ((,class (:background ,current-line :foreground ,foreground))))

     (border ((,class (:background ,current-line))))
     (border-glyph ((,class (nil))))

     (highlight ((,class (:inverse-video nil :background ,current-line))))

     (gui-element ((,class (:background ,current-line :foreground ,foreground))))

     (header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

     (link ((,class (:foreground nil :underline t))))
     (widget-button ((,class (:underline t))))
     (widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

     ;; Mode line
     (mode-line ((,class (:background ,black ; for powerline (previously far-background)
                          :foreground ,foreground))))
     (mode-line-buffer-id ((,class (:foreground ,foreground :background nil))))
     (mode-line-inactive ((,class (:inherit mode-line
                                   :foreground ,subtle
                                   :background ,black; for powerline (previously far-background)
                                   :weight normal
                                   :box nil))))
     (mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
     (mode-line-highlight ((,class (:foreground ,purple :box nil))))
     (minibuffer-prompt ((,class (:foreground ,blue))))

     ;; Powerline
     (exordium-powerline-active1 ((,class (:background ,black))))
     (exordium-powerline-active2 ((,class (:background ,current-line))))
     (exordium-powerline-active3 ((,class (:background ,orange :foreground ,background))))
     (exordium-powerline-active4 ((,class (:background ,red :foreground ,background))))
     (exordium-powerline-active5 ((,class (:background ,green :foreground ,background))))
     (exordium-powerline-inactive1 ((,class (:background ,black :foreground ,comment))))
     (exordium-powerline-inactive2 ((,class (:background ,current-line :foreground ,comment))))
     (exordium-powerline-inactive3 ((,class (:background ,comment :foreground ,background))))
     (exordium-project-name ((,class (:foreground ,purple))))

     (powerline-active1 ((t (:foreground ,foreground :background ,selection))))
     (powerline-active2 ((t (:foreground ,foreground :background ,"#78909c"))))
     (powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
     (powerline-inactive2 ((t (:foreground ,comment :background ,selection))))

     ;; Search
     (match ((,class (:foreground ,selection :background ,current-line :inverse-video nil))))
     (isearch ((,class (:foreground ,selection :background ,current-line))))
     (isearch-lazy-highlight-face ((,class (:foreground ,selection :background nil :inverse-video nil))))
     (isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,purple))))
     (ido-first-match ((,class (:foreground ,orange))))
     (ido-only-match ((,class (:foreground ,green))))
     (ido-indicator ((,class (:foreground ,red :background ,background))))
     (ido-virtual ((,class (:foreground ,comment))))

     ;; flx-ido
     (flx-highlight-face ((,class (:inherit nil :foreground ,yellow :weight bold :underline nil))))

     ;; Helm
     (helm-header ((,class (:foreground ,foreground :background ,background))))
     (helm-selection ((,class (:background ,current-line :foreground ,yellow))))
     (helm-ff-file ((,class (:foreground ,foreground ))))
     (helm-ff-directory ((,class (:foreground ,aqua ))))
     (helm-ff-executable ((,class (:foreground ,green ))))
     (helm-buffer-directory ((,class (:foreground ,aqua))))
     (helm-buffer-file ((,class (:foreground ,foreground))))
     (helm-grep-file ((,class (:foreground ,aqua :underline t))))
     (helm-buffer-process ((,class (:foreground ,red))))
     (helm-buffer-not-saved ((,class (:foreground ,orange))))
     (helm-candidate-number ((,class (:foreground ,foreground :background ,"#ef6c00"))))
     (helm-source-header ((,class (:background ,"#455A64" :foreground ,"#eceff1" :height 1.3 :bold t ))))

     ;; Flycheck
     (flycheck-error ((,class (:underline (:style wave :color ,red)))))
     (flycheck-warning ((,class (:underline (:style wave :color ,orange)))))

     ;; Flymake
     (flymake-warnline ((,class (:underline (:style wave :color ,orange) :background ,background))))
     (flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue))))
     (compilation-mode-line-exit ((,class (:foreground ,green))))
     (compilation-mode-line-fail ((,class (:foreground ,red))))
     (compilation-mode-line-run ((,class (:foreground ,blue))))

     ;; RTags
     ;; Loud:
     ;;(rtags-errline ((,class (:background ,red :foreground ,background))))
     ;;(rtags-warnline ((,class (:background ,orange :foreground ,background))))
     ;;(rtags-fixitline ((,class (:background ,green :foreground ,background))))
     (rtags-errline ((,class (:underline (:color ,red :style wave)))))
     (rtags-warnline ((,class (:underline (:color ,orange :style wave)))))
     (rtags-fixitline ((,class (:underline (:color ,green :style wave)))))

     ;; Magit
     (magit-branch ((,class (:foreground ,green))))
     (magit-diff-add ((,class (:inherit diff-added))))
     (magit-diff-del ((,class (:inherit diff-removed))))
     (magit-header ((,class (:inherit nil :weight bold))))
     (magit-item-highlight ((,class (:inherit highlight :background nil))))
     (magit-log-author ((,class (:foreground ,aqua))))
     (magit-log-graph ((,class (:foreground ,comment))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
     (magit-log-sha1 ((,class (:foreground ,yellow))))
     (magit-section-title ((,class (:foreground ,blue :weight bold))))

     ;; git-gutter
     (git-gutter:modified ((,class (:foreground ,purple :weight bold))))
     (git-gutter:added ((,class (:foreground ,green :weight bold))))
     (git-gutter:deleted ((,class (:foreground ,red :weight bold))))
     (git-gutter:unchanged ((,class (:background ,yellow))))

     ;; git-gutter-fringe
     (git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
     (git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
     (git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

     ;; Diff
     (diff-added ((,class (:foreground ,green))))
     (diff-changed ((,class (:foreground ,purple))))
     (diff-removed ((,class (:foreground ,orange))))
     (diff-header ((,class (:foreground ,aqua :background nil))))
     (diff-file-header ((,class (:foreground ,blue :background nil))))
     (diff-hunk-header ((,class (:foreground ,purple))))
     (diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
     (diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

     ;; Ediff
     (ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
     (ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
     (ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
     (ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

     ;; Grep
     (grep-context-face ((,class (:foreground ,comment))))
     (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     (regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

     ;; Org
     (org-agenda-structure ((,class (:foreground ,aqua :bold t))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-done ((,class (:foreground ,green))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     (org-block ((,class (:foreground ,orange))))
     (org-block-background ((,class (:background "#1F2232"))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,current-line))))
     (org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     (org-date ((,class (:foreground ,"#80cbc4" :underline t))))
     (org-document-info ((,class (:foreground ,aqua :height 1.35))))
     (org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
     (org-document-title ((,class (:weight bold :foreground ,orange :height 1.35))))
     (org-done ((,class (:foreground ,green :bold t :background,"#1b5e20"))))
     (org-ellipsis ((,class (:foreground ,comment))))
     (org-footnote ((,class (:foreground ,aqua))))
     (org-formula ((,class (:foreground ,red))))
     (org-hide ((,class (:foreground ,background :background ,background))))
     (org-link ((,class (:foreground ,blue :underline t))))
     (org-scheduled ((,class (:foreground ,green))))
     (org-scheduled-previously ((,class (:foreground ,orange))))
     (org-scheduled-today ((,class (:foreground ,green))))
     (org-special-keyword ((,class (:foreground ,comment))))
     (org-table ((,class (:foreground ,"#e3f2fd" :background ,far-background))))
     (org-todo ((,class (:foreground ,"#ffab91" :bold t :background ,"#dd2c00"))))
     (org-upcoming-deadline ((,class (:foreground ,orange))))
     (org-warning ((,class (:weight bold :foreground ,red))))
     (org-block-begin-line ((,class (:foreground ,"#b3e5fc" :underline ,"#e1f5fe"))))
     (org-block-end-line ((,class (:foreground ,"#b3e5fc" :overline ,"#e1f5fe"))))

     (org-level-1 ((,class (:inherit nil
                            :overline ,"#b0bec5"
                            :foreground ,"#eceff1"
                            :background ,"#455A64"
                            :weight bold
                            :height 1.3))))
     (org-level-2 ((,class (:inherit nil
                            :foreground ,"#e1f5fe"
                            :background ,"#21575b"
                            :overline ,"#e1f5fe"
                            :height 1.2))))
     (org-level-3 ((,class (:inherit nil :foreground ,"#a5d6a7" :height 1.1))))
     (org-level-4 ((,class (:inherit nil :foreground ,"#ffcc80" :height 1.0))))
     (org-level-5 ((,class (:inherit nil :foreground ,"#b3e5fc"))))
     (org-level-6 ((,class (:inherit nil :foreground ,"CadetBlue1"))))
     (org-level-7 ((,class (:inherit nil :foreground ,"aquamarine1"))))
     (org-level-8 ((,class (:inherit nil :foreground ,purple))))
     (org-level-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))

     ;; Outline
     (outline-1 ((,class (:inherit nil :foreground ,"#cfd8dc"))))
     (outline-2 ((,class (:inherit nil :foreground ,"#b0bec5"))))
     (outline-3 ((,class (:inherit nil :foreground ,"#a5d6a7" ))))
     (outline-4 ((,class (:inherit nil :foreground ,"#ffcc80" ))))
     (outline-5 ((,class (:inherit nil :foreground ,"#b3e5fc"))))
     (outline-6 ((,class (:inherit nil :foreground ,"CadetBlue1"))))
     (outline-7 ((,class (:inherit nil :foreground ,"aquamarine1"))))
     (outline-8 ((,class (:inherit nil :foreground ,purple))))
     (outline-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))

     ;; ansi-term
     (term ((,class (:foreground nil :background nil :inherit default))))
     (term-color-black ((,class (:foreground ,foreground :background ,foreground))))
     (term-color-red ((,class (:foreground ,red :background ,red))))
     (term-color-green ((,class (:foreground ,green :background ,green))))
     (term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
     (term-color-blue ((,class (:foreground ,blue :background ,blue))))
     (term-color-magenta ((,class (:foreground ,purple :background ,purple))))
     (term-color-cyan ((,class (:foreground ,aqua :background ,aqua))))
     (term-color-white ((,class (:foreground ,background :background ,background))))

     ;; Markdown
     (markdown-url-face ((,class (:inherit link))))
     (markdown-link-face ((,class (:foreground ,blue :underline t))))

     ;; js2-mode
     (js2-warning ((,class (:underline ,orange))))
     (js2-error ((,class (:foreground nil :underline ,red))))
     (js2-external-variable ((,class (:foreground ,purple))))
     (js2-function-param ((,class (:foreground ,blue))))
     (js2-instance-member ((,class (:foreground ,blue))))
     (js2-private-function-call ((,class (:foreground ,red))))

     ;; nxml
     (nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((,class (:underline ,red))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; White spaces
     (trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-line ((,class (:background nil :foreground ,red))))
     (whitespace-indentation ((,class (:background nil :foreground ,aqua))))
     (whitespace-space ((,class (:background nil :foreground ,selection))))
     (whitespace-newline ((,class (:background nil :foreground ,selection))))
     (whitespace-tab ((,class (:background nil :foreground ,selection))))
     (whitespace-hspace ((,class (:background nil :foreground ,selection))))

     ;; Clojure
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,foreground))))
     (clojure-braces ((,class (:foreground ,green))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,aqua :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,purple))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,aqua))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,aqua))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; Slime
     (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:weight bold))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((,class (:foreground ,green))))
     (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     ;; Other
     (csv-separator-face ((,class (:foreground ,orange))))

     (iedit-occurrence ((,class (:foreground ,foreground :background ,orange))))

     (which-func ((,class (:foreground ,blue :background nil))))

     ;;(hl-sexp-face ((,class (:background ,current-line))))
     (highlight-symbol-face ((,class (:background ,selection))))
     (highlight-80+ ((,class (:background ,current-line))))
     )))

(defun define-material-theme ()
  "Define the material theme"
  (deftheme material "A theme based on the colors of the Google Material Design")
  (with-material-colors
   (apply 'custom-theme-set-faces 'material (material-face-specs)))
  (provide-theme 'material))

;; Extra functions

(defun set-material-extra-org-statuses ()
  (require 'org)
  (with-material-colors
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,yellow
                      :foreground ,yellow :background "#dd2c00"
                      :weight bold :box nil))
           ("WAIT" . (;:background ,orange
                      :foreground ,orange :background "#dd2c00"
                      :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-material ()
  "Sets the colors to the material theme"
  (interactive)
  (with-material-colors
   (apply 'custom-set-faces (material-face-specs))))

(provide 'color-theme-material)

;;; material-theme.el ends here
