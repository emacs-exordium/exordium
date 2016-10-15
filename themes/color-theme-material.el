;;; color-theme-material.el --- based on the colors of the Google Material Design
;;;
;;; Credit:
;;; Inspired by the theme from Paulik Christoph
;;; See https://github.com/cpaulik/emacs-material-theme
;;;
;;; The colors in this theme should be defined from the Material palette: see
;;; http://www.google.com/design/spec/style/color.html

(require 'org)
(require 'init-prefs)

;;; Themes options

(defcustom exordium-material-italics nil
  "Enable italics for certain faces, such as comments"
  :group 'exordium
  :type  'boolean)

;;; Color palette

(defconst material-colors
  '((background          . "#263238")  ; = Blue Gray 900
    (current-line        . "#37474f")  ; = Blue Gray 800
    (far-background      . "#1c1f26")
    (subtle              . "#a7adba")
    (selection           . "#555555")
    (secondary-selection . "#bf616a")
    (foreground          . "#ffffff")
    (comment             . "#b0bec5")
    (red                 . "#f36c60")
    (orange              . "#ff9800")
    (orange-200          . "#ffcc80")
    (deep-orange-200     . "#ffab91")
    (deep-orange-a700    . "#DD2C00")
    (yellow              . "#fff59d")
    (yellow-600          . "#fdd835")
    (light-green-500     . "#8bc34a")
    (light-green-400     . "#9ccc65")
    (green-200           . "#a5d6a7")
    (green-900           . "#1b5e20")
    (light-blue-200      . "#81d4fa")
    (light-blue-100      . "#b3e5fc")
    (light-blue-50       . "#e1f5fe")
    (blue                . "#4dd0e1")
    (cyan-a100           . "#84ffff")
    (teal-200            . "#80cbc4")
    (blue-gray-50        . "#eceff1")
    (blue-gray-100       . "#cfd8dc")
    (blue-gray-700       . "#455a64")
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
     (default ((t (:foreground ,foreground :background ,background))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:slant italic :weight bold))))
     (underline ((t (:underline t))))
     (italic ((t (:slant italic))))
     (shadow ((t (:background ,far-background))))
     (success ((t (:foreground ,light-green-500))))
     (error ((t (:foreground ,red))))
     (warning ((t (:foreground ,orange))))
     (scroll-bar ((t (:background ,background))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((t (:background ,blue :foreground ,current-line))))
     (show-paren-mismatch ((t (:background ,red :foreground ,current-line))))

     ;; Region
     (region ((t (:background ,selection))))
     (secondary-selection ((t (:background ,secondary-selection))))

     ;; Font lock
     (font-lock-builtin-face ((t (:foreground ,light-blue-200))))
     (font-lock-comment-delimiter-face ((t (:foreground ,comment
                                                 :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-comment-face ((t (:foreground ,comment
                                       :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-constant-face ((t (:foreground ,light-green-500))))
     (font-lock-doc-face ((t (:foreground ,comment
                                   :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-function-name-face ((t (:foreground ,light-blue-100))))
     (font-lock-keyword-face ((t (:foreground ,yellow))))
     (font-lock-negation-char-face ((t (:foreground ,blue))))
     (font-lock-preprocessor-face ((t (:foreground ,yellow-600))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,purple))))
     (font-lock-string-face ((t (:foreground ,light-green-400))))
     (font-lock-type-face ((t (:foreground ,cyan-a100))))
     (font-lock-variable-name-face ((t (:foreground ,yellow))))
     (font-lock-warning-face ((t (:weight bold :foreground ,red))))

     ;; Emacs interface
     (cursor ((t (:background ,orange))))
     (fringe ((t (:background ,far-background))))

     (linum ((t (:background ,background :foreground ,subtle
                      :underline nil :weight normal :box nil :slant normal))))
     (linum-highlight-face ((t (:background ,current-line :foreground ,foreground))))

     (border ((t (:background ,current-line))))
     (border-glyph ((t (nil))))

     (hl-line ((t (:background ,current-line :inherit nil))))
     (highlight ((t (:background ,light-green-500 :foreground ,background)))) ;+:foreground

     (gui-element ((t (:background ,current-line :foreground ,foreground))))

     (header-line ((t (:inherit mode-line :foreground ,purple :background nil))))

     (link ((t (:foreground nil :underline t))))
     (widget-button ((t (:underline t))))
     (widget-field ((t (:background ,current-line :box (:line-width 1 :color ,foreground)))))
     (menu ((t (:foreground ,foreground :background ,background))))

     ;; Mode line
     (mode-line ((t (:background ,black ; for powerline (previously far-background)
                          :foreground ,foreground))))
     (mode-line-buffer-id ((t (:foreground ,foreground :background nil))))
     (mode-line-inactive ((t (:inherit mode-line
                                   :foreground ,subtle
                                   :background ,black; for powerline (previously far-background)
                                   :weight normal
                                   :box nil))))
     (mode-line-emphasis ((t (:foreground ,foreground :slant italic))))
     (mode-line-highlight ((t (:foreground ,purple :box nil))))
     (minibuffer-prompt ((t (:foreground ,blue))))
     (which-func ((t (:foreground ,background :weight bold))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,black))))
     (exordium-powerline-active2 ((t (:background ,current-line))))
     (exordium-powerline-active3 ((t (:background ,orange :foreground ,background))))
     (exordium-powerline-active4 ((t (:background ,red :foreground ,background))))
     (exordium-powerline-active5 ((t (:background ,light-green-500 :foreground ,background))))
     (exordium-powerline-inactive1 ((t (:background ,black :foreground ,comment))))
     (exordium-powerline-inactive2 ((t (:background ,current-line :foreground ,comment))))
     (exordium-powerline-inactive3 ((t (:background ,comment :foreground ,background))))
     (exordium-project-name ((t (:foreground ,purple))))

     (powerline-active1 ((t (:foreground ,foreground :background ,selection))))
     (powerline-active2 ((t (:foreground ,foreground :background ,light-green-400))))
     (powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
     (powerline-inactive2 ((t (:foreground ,comment :background ,selection))))

     ;; Search
     (match ((t (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((t (:foreground ,yellow :background ,background :inverse-video t :inherit nil))))
     (lazy-highlight
      ((t (:foreground ,light-blue-200 :background ,background :inverse-video t))))
     (isearch-fail
      ((t (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,yellow :background ,background :inverse-video t))))
     (hi-pink ((t (:foreground ,purple :background ,background :inverse-video t))))
     (hi-green ((t (:foreground ,light-green-500 :background ,background :inverse-video t))))
     (hi-blue ((t (:foreground ,light-blue-200 :background ,background :inverse-video t))))

     ;; IDO
     (ido-subdir ((t (:foreground ,purple))))
     (ido-first-match ((t (:foreground ,orange))))
     (ido-only-match ((t (:foreground ,light-green-500))))
     (ido-indicator ((t (:foreground ,red :background ,background))))
     (ido-virtual ((t (:foreground ,comment))))

     ;; flx-ido
     (flx-highlight-face ((t (:inherit nil :foreground ,yellow :weight bold :underline nil))))

     ;; Helm
     (helm-header ((t (:foreground ,light-green-500 :background ,background
                            :underline nil :box nil))))
     (helm-source-header ((t (:foreground ,background :background ,orange
                                   :underline nil :weight bold :box nil))))
     (helm-selection ((t (:background ,selection :underline nil))))
     (helm-selection-line ((t (:background ,selection))))
     (helm-visible-mark ((t (:foreground ,background :background ,yellow))))
     (helm-candidate-number ((t (:foreground ,light-green-500 :background ,selection))))
     (helm-swoop-target-line-face ((t (:foreground ,background :background ,yellow))))
     (helm-swoop-target-word-face ((t (:foreground ,background :background ,light-blue-200))))

     ;; Flycheck
     (flycheck-error ((t (:underline (:style wave :color ,red)))))
     (flycheck-warning ((t (:underline (:style wave :color ,orange)))))

     ;; Flymake
     (flymake-warnline ((t (:underline (:style wave :color ,orange) :background ,background))))
     (flymake-errline ((t (:underline (:style wave :color ,red) :background ,background))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((t (:foreground ,yellow))))
     (compilation-line-number ((t (:foreground ,yellow))))
     (compilation-message-face ((t (:foreground ,blue))))
     (compilation-mode-line-exit ((t (:foreground ,light-green-500))))
     (compilation-mode-line-fail ((t (:foreground ,red))))
     (compilation-mode-line-run ((t (:foreground ,blue))))

     ;; RTags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,red :foreground ,background)
                                `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,orange :foreground ,background)
                                 `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,light-green-500 :foreground ,background)
                                  `(:underline (:color ,light-green-500 :style wave))))))
     (rtags-skippedline ((t :background ,far-background)))

     ;; Magit
     (magit-branch ((t (:foreground ,light-green-500))))
     (magit-diff-add ((t (:inherit diff-added))))
     (magit-diff-del ((t (:inherit diff-removed))))
     (magit-header ((t (:inherit nil :weight bold))))
     (magit-item-highlight ((t (:inherit hl-line :background nil))))
     (magit-log-author ((t (:foreground ,light-blue-200))))
     (magit-log-graph ((t (:foreground ,comment))))
     (magit-log-head-label-bisect-bad ((t (:foreground ,red))))
     (magit-log-head-label-bisect-good ((t (:foreground ,light-green-500))))
     (magit-log-head-label-default ((t (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((t (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-remote ((t (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-tags ((t (:foreground ,light-blue-200 :box nil :weight bold))))
     (magit-log-sha1 ((t (:foreground ,yellow))))
     (magit-section-title ((t (:foreground ,blue :weight bold))))

     ;; git-gutter
     (git-gutter:modified ((t (:foreground ,purple :weight bold))))
     (git-gutter:added ((t (:foreground ,light-green-500 :weight bold))))
     (git-gutter:deleted ((t (:foreground ,red :weight bold))))
     (git-gutter:unchanged ((t (:background ,yellow))))

     ;; git-gutter-fringe
     (git-gutter-fr:modified ((t (:foreground ,purple :weight bold))))
     (git-gutter-fr:added ((t (:foreground ,light-green-500 :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))

     ;; Diff
     (diff-added ((t (:foreground ,light-green-500))))
     (diff-changed ((t (:foreground ,purple))))
     (diff-removed ((t (:foreground ,orange))))
     (diff-header ((t (:foreground ,light-blue-200 :background nil))))
     (diff-file-header ((t (:foreground ,blue :background nil))))
     (diff-hunk-header ((t (:foreground ,purple))))
     (diff-refine-added ((t (:inherit diff-added :inverse-video t))))
     (diff-refine-removed ((t (:inherit diff-removed :inverse-video t))))

     ;; Ediff
     (ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-odd-diff-A  ((t (:foreground ,comment :background nil :inverse-video t))))
     (ediff-odd-diff-B  ((t (:foreground ,comment :background nil :inverse-video t))))

     ;; Grep
     (grep-context-face ((t (:foreground ,comment))))
     (grep-error-face ((t (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,blue))))
     (grep-match-face ((t (:foreground nil :background nil :inherit match))))

     (regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

     ;; Org
     (org-agenda-structure ((t (:foreground ,light-blue-200 :bold t))))
     (org-agenda-date ((t (:foreground ,blue :underline nil))))
     (org-agenda-done ((t (:foreground ,light-green-500))))
     (org-agenda-dimmed-todo-face ((t (:foreground ,comment))))
     (org-block ((t (:foreground ,orange))))
     (org-code ((t (:foreground ,yellow))))
     (org-column ((t (:background ,current-line))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground ,teal-200 :underline t))))
     (org-document-info ((t
                          ,(append `(:foreground ,light-blue-200)
                                   (if exordium-theme-use-big-font '(:height 1.35) nil)))))
     (org-document-info-keyword ((t
                                  ,(append `(:foreground ,light-green-500)
                                           (if exordium-theme-use-big-font '(:height 1.35) nil)))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,orange)
                                    (if exordium-theme-use-big-font '(:height 1.35))))))
     (org-done ((t (:foreground ,light-green-500 :bold t :background ,green-900))))
     (org-ellipsis ((t (:foreground ,comment))))
     (org-footnote ((t (:foreground ,light-blue-200))))
     (org-formula ((t (:foreground ,red))))
     (org-hide ((t (:foreground ,background :background ,background))))
     (org-link ((t (:foreground ,blue :underline t))))
     (org-scheduled ((t (:foreground ,light-green-500))))
     (org-scheduled-previously ((t (:foreground ,orange))))
     (org-scheduled-today ((t (:foreground ,light-green-500))))
     (org-special-keyword ((t (:foreground ,comment))))
     (org-table ((t (:foreground ,light-blue-50 :background ,far-background))))
     (org-todo ((t (:foreground ,deep-orange-200 :bold t :background ,deep-orange-a700))))
     (org-upcoming-deadline ((t (:foreground ,orange))))
     (org-warning ((t (:weight bold :foreground ,red))))
     (org-block-begin-line ((t (:foreground ,light-blue-100 :underline ,light-blue-50))))
     (org-block-end-line ((t (:foreground ,light-blue-100 :overline ,light-blue-50))))

     (org-level-1 ((t ,(append `(:inherit nil
                                      :overline ,comment
                                      :foreground ,blue-gray-50
                                      :background ,blue-gray-700
                                      :weight bold)
                                    (if exordium-theme-use-big-font '(:height 1.3) nil)))))
     (org-level-2 ((t ,(append `(:inherit nil
                                      :foreground ,light-blue-50
                                      :background "#21575b"
                                      :overline ,light-blue-50)
                                    (if exordium-theme-use-big-font '(:height 1.2) nil)))))
     (org-level-3 ((t ,(append `(:inherit nil :foreground ,green-200)
                                    (if exordium-theme-use-big-font '(:height 1.1) nil)))))
     (org-level-4 ((t (:inherit nil :foreground ,orange-200))))
     (org-level-5 ((t (:inherit nil :foreground ,light-blue-100))))
     (org-level-6 ((t (:inherit nil :foreground ,cyan-a100))))
     (org-level-7 ((t (:inherit nil :foreground ,teal-200))))
     (org-level-8 ((t (:inherit nil :foreground ,purple))))
     (org-level-9 ((t (:inherit nil :foreground ,blue-gray-100))))

     ;; Outline
     (outline-1 ((t (:inherit nil :foreground ,blue-gray-100))))
     (outline-2 ((t (:inherit nil :foreground "#b0bec5"))))
     (outline-3 ((t (:inherit nil :foreground ,green-200))))
     (outline-4 ((t (:inherit nil :foreground ,orange-200))))
     (outline-5 ((t (:inherit nil :foreground ,light-blue-100))))
     (outline-6 ((t (:inherit nil :foreground ,cyan-a100))))
     (outline-7 ((t (:inherit nil :foreground ,teal-200))))
     (outline-8 ((t (:inherit nil :foreground ,purple))))
     (outline-9 ((t (:inherit nil :foreground ,blue-gray-100))))

     ;; ansi-term
     (term ((t (:foreground nil :background nil :inherit default))))
     (term-color-black ((t (:foreground ,foreground :background ,foreground))))
     (term-color-red ((t (:foreground ,red :background ,red))))
     (term-color-green ((t (:foreground ,light-green-500 :background ,light-green-500))))
     (term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
     (term-color-blue ((t (:foreground ,blue :background ,blue))))
     (term-color-magenta ((t (:foreground ,purple :background ,purple))))
     (term-color-cyan ((t (:foreground ,light-blue-200 :background ,light-blue-200))))
     (term-color-white ((t (:foreground ,background :background ,background))))

     ;; Markdown
     (markdown-url-face ((t (:inherit link :foreground ,blue-gray-100))))
     (markdown-link-face ((t (:foreground ,blue :underline t))))
     (markdown-header-face-1 ((t
                               ,(append `(:weight bold :foreground ,light-blue-100)
                                        (if exordium-theme-use-big-font '(:height 1.44)) nil))))
     (markdown-header-face-2 ((t
                               ,(append `(:weight bold :foreground ,light-blue-100)
                                        (if exordium-theme-use-big-font '(:height 1.2)) nil))))

     ;; js2-mode
     (js2-warning ((t (:underline ,orange :style wave))))
     (js2-error ((t (:foreground nil :underline ,red :style wave))))
     (js2-external-variable ((t (:foreground ,purple))))
     (js2-function-param ((t (:foreground ,blue))))
     (js2-instance-member ((t (:foreground ,blue))))
     (js2-private-function-call ((t (:foreground ,red))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,orange :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,red :style wave)))))

     ;; nxml
     (nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((t (:underline ,red))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((t (:foreground ,foreground))))
     (undo-tree-visualizer-current-face ((t (:foreground ,light-green-500 :weight bold))))
     (undo-tree-visualizer-active-branch-face ((t (:foreground ,red))))
     (undo-tree-visualizer-register-face ((t (:foreground ,yellow))))

     ;; White spaces
     (trailing-whitespace ((t (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-trailing ((t (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-after-tab ((t (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-before-tab ((t (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-empty ((t (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-line ((t (:background nil :foreground ,red))))
     (whitespace-indentation ((t (:background nil :foreground ,light-blue-200))))
     (whitespace-space ((t (:background nil :foreground ,selection))))
     (whitespace-newline ((t (:background nil :foreground ,selection))))
     (whitespace-tab ((t (:background nil :foreground ,selection))))
     (whitespace-hspace ((t (:background nil :foreground ,selection))))

     ;; Clojure
     (clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((t (:background nil :foreground nil :underline ,light-green-500))))

     (clojure-keyword ((t (:foreground ,yellow))))
     (clojure-parens ((t (:foreground ,foreground))))
     (clojure-braces ((t (:foreground ,light-green-500))))
     (clojure-brackets ((t (:foreground ,yellow))))
     (clojure-double-quote ((t (:foreground ,light-blue-200 :background nil))))
     (clojure-special ((t (:foreground ,blue))))
     (clojure-java-call ((t (:foreground ,purple))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

     (eval-sexp-fu-flash ((t (:background ,orange :foreground ,background))))
     (eval-sexp-fu-flash-error ((t (:background ,deep-orange-200 :foreground ,background))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,foreground))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,light-blue-200))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,light-green-500))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,foreground))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,light-blue-200))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,light-green-500))))
     (rainbow-delimiters-unmatched-face ((t (:foreground ,red))))

     ;; Slime
     (sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((t (:weight bold))))
     (slime-repl-input-face ((t (:weight normal :underline nil))))
     (slime-repl-prompt-face ((t (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((t (:foreground ,light-green-500))))
     (slime-repl-output-face ((t (:foreground ,blue :background ,background))))

     ;; Other
     (csv-separator-face ((t (:foreground ,orange))))

     (iedit-occurrence ((t (:foreground ,foreground :background ,orange))))

     (which-func ((t (:foreground ,blue :background nil))))

     ;;(hl-sexp-face ((t (:background ,current-line))))
     (highlight-symbol-face ((t (:background ,selection))))
     (highlight-80+ ((t (:background ,current-line))))
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
                      :foreground ,yellow :background ,deep-orange-a700
                      :weight bold :box nil))
           ("WAIT" . (;:background ,orange
                      :foreground ,orange :background ,deep-orange-a700
                      :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-material ()
  "Sets the colors to the material theme"
  (interactive)
  (with-material-colors
   (apply 'custom-set-faces (material-face-specs))))

(provide 'color-theme-material)

;;; material-theme.el ends here

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
