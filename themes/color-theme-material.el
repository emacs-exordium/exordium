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
     (default ((,class (:foreground ,foreground :background ,background))))
     (bold ((,class (:weight bold))))
     (bold-italic ((,class (:slant italic :weight bold))))
     (underline ((,class (:underline t))))
     (italic ((,class (:slant italic))))
     (shadow ((,class (:background ,current-line))))
     (success ((,class (:foreground ,light-green-500))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange))))
     (scroll-bar ((,class (:background ,background))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:background ,blue :foreground ,current-line))))
     (show-paren-mismatch ((,class (:background ,red :foreground ,current-line))))

     ;; Region
     (region ((,class (:background ,selection))))
     (secondary-selection ((,class (:background ,secondary-selection))))

     ;; Font lock
     (font-lock-builtin-face ((,class (:foreground ,light-blue-200))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,comment
                                                 :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-comment-face ((,class (:foreground ,comment
                                       :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-constant-face ((,class (:foreground ,light-green-500))))
     (font-lock-doc-face ((,class (:foreground ,comment
                                   :slant ,(if exordium-material-italics 'italic 'normal)))))
     (font-lock-doc-string-face ((,class (:foreground ,comment))))
     (font-lock-function-name-face ((,class (:foreground ,light-blue-100))))
     (font-lock-keyword-face ((,class (:foreground ,yellow))))
     (font-lock-negation-char-face ((,class (:foreground ,blue))))
     (font-lock-preprocessor-face ((,class (:foreground ,yellow-600))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
     (font-lock-string-face ((,class (:foreground ,light-green-400))))
     (font-lock-type-face ((,class (:foreground ,cyan-a100))))
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

     (hl-line ((,class (:background ,current-line :inherit nil))))
     (highlight ((,class (:background ,light-green-500 :foreground ,background)))) ;+:foreground

     (gui-element ((,class (:background ,current-line :foreground ,foreground))))

     (header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

     (link ((,class (:foreground nil :underline t))))
     (widget-button ((,class (:underline t))))
     (widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))
     (menu ((,class (:foreground ,foreground :background ,background))))

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
     (exordium-powerline-active5 ((,class (:background ,light-green-500 :foreground ,background))))
     (exordium-powerline-inactive1 ((,class (:background ,black :foreground ,comment))))
     (exordium-powerline-inactive2 ((,class (:background ,current-line :foreground ,comment))))
     (exordium-powerline-inactive3 ((,class (:background ,comment :foreground ,background))))
     (exordium-project-name ((,class (:foreground ,purple))))

     (powerline-active1 ((t (:foreground ,foreground :background ,selection))))
     (powerline-active2 ((t (:foreground ,foreground :background ,light-green-400))))
     (powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
     (powerline-inactive2 ((t (:foreground ,comment :background ,selection))))

     ;; Search
     (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
     (isearch-lazy-highlight-face
      ((,class (:foreground ,light-blue-200 :background ,background :inverse-video t))))
     (isearch-fail
      ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,purple))))
     (ido-first-match ((,class (:foreground ,orange))))
     (ido-only-match ((,class (:foreground ,light-green-500))))
     (ido-indicator ((,class (:foreground ,red :background ,background))))
     (ido-virtual ((,class (:foreground ,comment))))

     ;; flx-ido
     (flx-highlight-face ((,class (:inherit nil :foreground ,yellow :weight bold :underline nil))))

     ;; Helm
     (helm-header ((,class (:foreground ,light-green-500 :background ,background
                            :underline nil :box nil))))
     (helm-source-header ((,class (:foreground ,background :background ,orange
                                   :underline nil :weight bold :box nil))))
     (helm-selection ((,class (:background ,selection :underline nil))))
     (helm-selection-line ((,class (:background ,selection))))
     (helm-visible-mark ((,class (:foreground ,background :background ,yellow))))
     (helm-candidate-number ((,class (:foreground ,light-green-500 :background ,selection))))
     (helm-swoop-target-line-face ((,class (:foreground ,background :background ,yellow))))
     (helm-swoop-target-word-face ((,class (:foreground ,background :background ,light-blue-200))))

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
     (compilation-mode-line-exit ((,class (:foreground ,light-green-500))))
     (compilation-mode-line-fail ((,class (:foreground ,red))))
     (compilation-mode-line-run ((,class (:foreground ,blue))))

     ;; RTags
     (rtags-errline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,red :foreground ,background)
                                `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,orange :foreground ,background)
                                 `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,light-green-500 :foreground ,background)
                                  `(:underline (:color ,light-green-500 :style wave))))))

     ;; Magit
     (magit-branch ((,class (:foreground ,light-green-500))))
     (magit-diff-add ((,class (:inherit diff-added))))
     (magit-diff-del ((,class (:inherit diff-removed))))
     (magit-header ((,class (:inherit nil :weight bold))))
     (magit-item-highlight ((,class (:inherit hl-line :background nil))))
     (magit-log-author ((,class (:foreground ,light-blue-200))))
     (magit-log-graph ((,class (:foreground ,comment))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,light-green-500))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-tags ((,class (:foreground ,light-blue-200 :box nil :weight bold))))
     (magit-log-sha1 ((,class (:foreground ,yellow))))
     (magit-section-title ((,class (:foreground ,blue :weight bold))))

     ;; git-gutter
     (git-gutter:modified ((,class (:foreground ,purple :weight bold))))
     (git-gutter:added ((,class (:foreground ,light-green-500 :weight bold))))
     (git-gutter:deleted ((,class (:foreground ,red :weight bold))))
     (git-gutter:unchanged ((,class (:background ,yellow))))

     ;; git-gutter-fringe
     (git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
     (git-gutter-fr:added ((,class (:foreground ,light-green-500 :weight bold))))
     (git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

     ;; Diff
     (diff-added ((,class (:foreground ,light-green-500))))
     (diff-changed ((,class (:foreground ,purple))))
     (diff-removed ((,class (:foreground ,orange))))
     (diff-header ((,class (:foreground ,light-blue-200 :background nil))))
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
     (org-agenda-structure ((,class (:foreground ,light-blue-200 :bold t))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-done ((,class (:foreground ,light-green-500))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     (org-block ((,class (:foreground ,orange))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,current-line))))
     (org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     (org-date ((,class (:foreground ,teal-200 :underline t))))
     (org-document-info ((,class
                          ,(append `(:foreground ,light-blue-200)
                                   (if exordium-theme-use-big-org-fonts '(:height 1.35) nil)))))
     (org-document-info-keyword ((,class
                                  ,(append `(:foreground ,light-green-500)
                                           (if exordium-theme-use-big-org-fonts '(:height 1.35) nil)))))
     (org-document-title ((,class
                           ,(append `(:weight bold :foreground ,orange)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.35))))))
     (org-done ((,class (:foreground ,light-green-500 :bold t :background ,green-900))))
     (org-ellipsis ((,class (:foreground ,comment))))
     (org-footnote ((,class (:foreground ,light-blue-200))))
     (org-formula ((,class (:foreground ,red))))
     (org-hide ((,class (:foreground ,background :background ,background))))
     (org-link ((,class (:foreground ,blue :underline t))))
     (org-scheduled ((,class (:foreground ,light-green-500))))
     (org-scheduled-previously ((,class (:foreground ,orange))))
     (org-scheduled-today ((,class (:foreground ,light-green-500))))
     (org-special-keyword ((,class (:foreground ,comment))))
     (org-table ((,class (:foreground ,light-blue-50 :background ,far-background))))
     (org-todo ((,class (:foreground ,deep-orange-200 :bold t :background ,deep-orange-a700))))
     (org-upcoming-deadline ((,class (:foreground ,orange))))
     (org-warning ((,class (:weight bold :foreground ,red))))
     (org-block-begin-line ((,class (:foreground ,light-blue-100 :underline ,light-blue-50))))
     (org-block-end-line ((,class (:foreground ,light-blue-100 :overline ,light-blue-50))))

     (org-level-1 ((,class ,(append `(:inherit nil
                                      :overline ,comment
                                      :foreground ,blue-gray-50
                                      :background ,blue-gray-700
                                      :weight bold)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.3) nil)))))
     (org-level-2 ((,class ,(append `(:inherit nil
                                      :foreground ,light-blue-50
                                      :background "#21575b"
                                      :overline ,light-blue-50)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.2) nil)))))
     (org-level-3 ((,class ,(append `(:inherit nil :foreground ,green-200)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.1) nil)))))
     (org-level-4 ((,class (:inherit nil :foreground ,orange-200))))
     (org-level-5 ((,class (:inherit nil :foreground ,light-blue-100))))
     (org-level-6 ((,class (:inherit nil :foreground ,cyan-a100))))
     (org-level-7 ((,class (:inherit nil :foreground ,teal-200))))
     (org-level-8 ((,class (:inherit nil :foreground ,purple))))
     (org-level-9 ((,class (:inherit nil :foreground ,blue-gray-100))))

     ;; Outline
     (outline-1 ((,class (:inherit nil :foreground ,blue-gray-100))))
     (outline-2 ((,class (:inherit nil :foreground "#b0bec5"))))
     (outline-3 ((,class (:inherit nil :foreground ,green-200))))
     (outline-4 ((,class (:inherit nil :foreground ,orange-200))))
     (outline-5 ((,class (:inherit nil :foreground ,light-blue-100))))
     (outline-6 ((,class (:inherit nil :foreground ,cyan-a100))))
     (outline-7 ((,class (:inherit nil :foreground ,teal-200))))
     (outline-8 ((,class (:inherit nil :foreground ,purple))))
     (outline-9 ((,class (:inherit nil :foreground ,blue-gray-100))))

     ;; ansi-term
     (term ((,class (:foreground nil :background nil :inherit default))))
     (term-color-black ((,class (:foreground ,foreground :background ,foreground))))
     (term-color-red ((,class (:foreground ,red :background ,red))))
     (term-color-green ((,class (:foreground ,light-green-500 :background ,light-green-500))))
     (term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
     (term-color-blue ((,class (:foreground ,blue :background ,blue))))
     (term-color-magenta ((,class (:foreground ,purple :background ,purple))))
     (term-color-cyan ((,class (:foreground ,light-blue-200 :background ,light-blue-200))))
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
     (undo-tree-visualizer-current-face ((,class (:foreground ,light-green-500 :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; White spaces
     (trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-line ((,class (:background nil :foreground ,red))))
     (whitespace-indentation ((,class (:background nil :foreground ,light-blue-200))))
     (whitespace-space ((,class (:background nil :foreground ,selection))))
     (whitespace-newline ((,class (:background nil :foreground ,selection))))
     (whitespace-tab ((,class (:background nil :foreground ,selection))))
     (whitespace-hspace ((,class (:background nil :foreground ,selection))))

     ;; Clojure
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,light-green-500))))

     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,foreground))))
     (clojure-braces ((,class (:foreground ,light-green-500))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,light-blue-200 :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,purple))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,light-blue-200))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,light-green-500))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,light-blue-200))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,light-green-500))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; Slime
     (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:weight bold))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((,class (:foreground ,light-green-500))))
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
