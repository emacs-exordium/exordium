;;; color-theme-zenburn.el --- A low contrast color theme.
;;;
;;; Credits:
;;; Jani Nurminen created the original theme for vim on such this port
;;; is based.
;;; Inspired by the emacs theme by Bozhidar Batsov:
;;; https://github.com/bbatsov/zenburn-emacs

(require 'org)

;;; Color palette.
;;; `+N' suffixes indicate a color is lighter.
;;; `-N' suffixes indicate a color is darker.

(defconst zenburn-colors
  '((fg+1     . "#FFFFEF")
    (fg       . "#DCDCCC")
    (fg-1     . "#656555")
    (bg-2     . "#000000")
    (bg-1     . "#2B2B2B")
    (bg-05    . "#383838")
    (bg       . "#3F3F3F")
    (bg+05    . "#494949")
    (bg+1     . "#4F4F4F")
    (bg+2     . "#5F5F5F")
    (bg+3     . "#6F6F6F")
    (red+1    . "#DCA3A3")
    (red      . "#CC9393")
    (red-1    . "#BC8383")
    (red-2    . "#AC7373")
    (red-3    . "#9C6363")
    (red-4    . "#8C5353")
    (orange   . "#DFAF8F")
    (yellow   . "#F0DFAF")
    (yellow-1 . "#E0CF9F")
    (yellow-2 . "#D0BF8F")
    (green-1  . "#5F7F5F")
    (green    . "#7F9F7F")
    (green+1  . "#8FB28F")
    (green+2  . "#9FC59F")
    (green+3  . "#AFD8AF")
    (green+4  . "#BFEBBF")
    (cyan     . "#93E0E3")
    (blue+1   . "#94BFF3")
    (blue     . "#8CD0D3")
    (blue-1   . "#7CB8BB")
    (blue-2   . "#6CA0A3")
    (blue-3   . "#5C888B")
    (blue-4   . "#4C7073")
    (blue-5   . "#366060")
    (magenta  . "#DC8CC3")))

(defmacro with-zenburn-colors (&rest body)
  "Execute `BODY' in a scope with variables bound to the zenburn colors."
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   zenburn-colors))
     ,@body))

(defmacro zenburn-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; basic coloring
     (button ((,class (:underline t))))
     (link ((,class (:foreground ,yellow :underline t :weight bold))))
     (link-visited ((,class (:foreground ,yellow-2 :underline t :weight normal))))
     (default ((,class (:foreground ,fg :background ,bg))))
     (cursor ((,class (:foreground ,fg :background ,fg+1))))
     (escape-glyph ((,class (:foreground ,yellow :bold t))))
     (fringe ((,class (:foreground ,fg :background ,bg+1))))
     (header-line ((,class (:foreground ,yellow :background ,bg-1
                            :box (:line-width -1 :style released-button)))))
     (highlight ((,class (:background ,bg-05))))
     (success ((,class (:foreground ,green :weight bold))))
     (warning ((,class (:foreground ,orange :weight bold))))

     ;; compilation
     (compilation-column-face ((,class (:foreground ,yellow))))
     (compilation-enter-directory-face ((,class (:foreground ,green))))
     (compilation-error-face ((,class (:foreground ,red-1 :weight bold :underline t))))
     (compilation-face ((,class (:foreground ,fg))))
     (compilation-info-face ((,class (:foreground ,blue))))
     (compilation-info ((,class (:foreground ,green+4 :underline t))))
     (compilation-leave-directory-face ((,class (:foreground ,green))))
     (compilation-line-face ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue))))
     (compilation-warning-face ((,class (:foreground ,orange :weight bold :underline t))))
     (compilation-mode-line-exit ((,class (:foreground ,green+2 :weight bold))))
     (compilation-mode-line-fail ((,class (:foreground ,red :weight bold))))
     (compilation-mode-line-run ((,class (:foreground ,yellow :weight bold))))

     ;; completions
     (completions-annotations ((,class (:foreground ,fg-1))))

     ;; grep
     (grep-context-face ((,class (:foreground ,fg))))
     (grep-error-face ((,class (:foreground ,red-1 :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground ,orange :weight bold))))
     (match ((,class (:background ,bg-1 :foreground ,orange :weight bold))))

     ;; isearch
     (isearch ((,class (:foreground ,yellow-2 :weight bold :background ,bg+2))))
     (isearch-fail ((,class (:foreground ,fg :background ,red-4))))
     (lazy-highlight ((,class (:foreground ,yellow-2 :weight bold :background ,bg-05))))

     ;; UI
     (menu ((,class (:foreground ,fg :background ,bg))))
     (minibuffer-prompt ((,class (:foreground ,yellow))))
     (mode-line
      ((,class (:foreground ,green+1 :background ,bg-1
                :box (:line-width -1 :style released-button)))
       (t :inverse-video t)))
     (mode-line-buffer-id ((,class (:foreground ,yellow :weight bold))))
     (mode-line-inactive
      ((,class (:foreground ,green-1 :background ,bg-05
                            :box (:line-width -1 :style released-button)))))
     (region ((,class (:background ,bg-1))
              (t :inverse-video t)))
     (secondary-selection ((,class (:background ,bg+2))))
     (trailing-whitespace ((,class (:background ,red))))
     (vertical-border ((,class (:foreground ,fg))))

     ;; font lock
     (font-lock-builtin-face ((,class (:foreground ,fg :weight bold))))
     (font-lock-comment-face ((,class (:foreground ,green :italic nil))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,green-1))))
     (font-lock-constant-face ((,class (:foreground ,green+4))))
     (font-lock-doc-face ((,class (:foreground ,green+2))))
     (font-lock-function-name-face ((,class (:foreground ,cyan))))
     (font-lock-keyword-face ((,class (:foreground ,yellow :weight bold))))
     (font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
     (font-lock-preprocessor-face ((,class (:foreground ,blue+1))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
     (font-lock-string-face ((,class (:foreground ,red))))
     (font-lock-type-face ((,class (:foreground ,blue-1))))
     (font-lock-variable-name-face ((,class (:foreground ,orange))))
     (font-lock-warning-face ((,class (:foreground ,yellow-2 :weight bold))))

     (c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;; Powerline
     (exordium-powerline-active1 ((,class (:background ,bg-1))))
     (exordium-powerline-active2 ((,class (:background ,bg-05))))
     (exordium-powerline-active3 ((,class (:background ,yellow :foreground ,bg))))
     (exordium-powerline-active4 ((,class (:background ,red :foreground ,bg))))
     (exordium-powerline-active5 ((,class (:background ,green :foreground ,bg))))
     (exordium-powerline-inactive1 ((,class (:background ,bg-1))))
     (exordium-powerline-inactive2 ((,class (:background ,bg-05))))
     (exordium-powerline-inactive3 ((,class (:background ,bg :foreground ,yellow))))
     (exordium-project-name ((,class (:foreground ,yellow))))

     (powerline-active1 ((,class (:background ,bg-05 :inherit mode-line))))
     (powerline-active2 ((,class (:background ,bg+2 :inherit mode-line))))
     (powerline-inactive1 ((,class (:background ,bg+1 :inherit mode-line-inactive))))
     (powerline-inactive2 ((,class (:background ,bg+3 :inherit mode-line-inactive))))

     ;; auctex
     (font-latex-bold-face ((,class (:inherit bold))))
     (font-latex-warning-face ((,class (:foreground nil :inherit font-lock-warning-face))))
     (font-latex-sectioning-5-face ((,class (:foreground ,red :weight bold ))))
     (font-latex-sedate-face ((,class (:foreground ,yellow))))
     (font-latex-italic-face ((,class (:foreground ,cyan :slant italic))))
     (font-latex-string-face ((,class (:inherit ,font-lock-string-face))))
     (font-latex-math-face ((,class (:foreground ,orange))))

     ;; auto-complete
     (ac-candidate-face ((,class (:background ,bg+3 :foreground ,bg-2))))
     (ac-selection-face ((,class (:background ,blue-4 :foreground ,fg))))
     (popup-tip-face ((,class (:background ,yellow-2 :foreground ,bg-2))))
     (popup-scroll-bar-foreground-face ((,class (:background ,blue-5))))
     (popup-scroll-bar-background-face ((,class (:background ,bg-1))))
     (popup-isearch-match ((,class (:background ,bg :foreground ,fg))))

     ;; clojure-test-mode
     (clojure-test-failure-face ((,class (:foreground ,orange :weight bold :underline t))))
     (clojure-test-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (clojure-test-success-face ((,class (:foreground ,green+1 :weight bold :underline t))))

     ;; diff
     (diff-added ((,class (:foreground ,green+4 :background nil))
                  (t (:foreground ,green-1 :background nil))))
     (diff-changed ((,class (:foreground ,yellow))))
     (diff-removed ((,class (:foreground ,red :background nil))
                    (t (:foreground ,red-3 :background nil))))
     (diff-refine-added ((,class (:inherit diff-added :weight bold))))
     (diff-refine-change ((,class (:inherit diff-changed :weight bold))))
     (diff-refine-removed ((,class (:inherit diff-removed :weight bold))))
     (diff-header ((,class (:background ,bg+2))
                   (t (:background ,fg :foreground ,bg))))
     (diff-file-header
      ((,class (:background ,bg+2 :foreground ,fg :bold t))
       (t (:background ,fg :foreground ,bg :bold t))))

     ;; ediff
     (ediff-current-diff-A ((,class (:foreground ,fg :background ,red-4))))
     (ediff-current-diff-Ancestor ((,class (:foreground ,fg :background ,red-4))))
     (ediff-current-diff-B ((,class (:foreground ,fg :background ,green-1))))
     (ediff-current-diff-C ((,class (:foreground ,fg :background ,blue-5))))
     (ediff-even-diff-A ((,class (:background ,bg+1))))
     (ediff-even-diff-Ancestor ((,class (:background ,bg+1))))
     (ediff-even-diff-B ((,class (:background ,bg+1))))
     (ediff-even-diff-C ((,class (:background ,bg+1))))
     (ediff-fine-diff-A ((,class (:foreground ,fg :background ,red-2 :weight bold))))
     (ediff-fine-diff-Ancestor ((,class (:foreground ,fg :background ,red-2 weight bold))))
     (ediff-fine-diff-B ((,class (:foreground ,fg :background ,green :weight bold))))
     (ediff-fine-diff-C ((,class (:foreground ,fg :background ,blue-3 :weight bold ))))
     (ediff-odd-diff-A ((,class (:background ,bg+2))))
     (ediff-odd-diff-Ancestor ((,class (:background ,bg+2))))
     (ediff-odd-diff-B ((,class (:background ,bg+2))))
     (ediff-odd-diff-C ((,class (:background ,bg+2))))

     ;; eshell
     (eshell-prompt ((,class (:foreground ,yellow :weight bold))))
     (eshell-ls-archive ((,class (:foreground ,red-1 :weight bold))))
     (eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     (eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     (eshell-ls-directory ((,class (:foreground ,blue+1 :weight bold))))
     (eshell-ls-executable ((,class (:foreground ,red+1 :weight bold))))
     (eshell-ls-unreadable ((,class (:foreground ,fg))))
     (eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     (eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     (eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     (eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))

     ;; flycheck
     (flycheck-error
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,red-1) :inherit unspecified))
       (t (:foreground ,red-1 :weight bold :underline t))))
     (flycheck-warning
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,yellow) :inherit unspecified))
       (t (:foreground ,yellow :weight bold :underline t))))
     (flycheck-info
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,cyan) :inherit unspecified))
       (t (:foreground ,cyan :weight bold :underline t))))
     (flycheck-fringe-error ((,class (:foreground ,red-1 :weight bold))))
     (flycheck-fringe-warning ((,class (:foreground ,yellow :weight bold))))
     (flycheck-fringe-info ((,class (:foreground ,cyan :weight bold))))

     ;; flymake
     (flymake-errline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,red)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,red-1 :weight bold :underline t))))
     (flymake-warnline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,orange)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,orange :weight bold :underline t))))
     (flymake-infoline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,green)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,green-1 :weight bold :underline t))))

     ;; flyspell
     (flyspell-duplicate
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,orange) :inherit unspecified))
       (t (:foreground ,orange :weight bold :underline t))))
     (flyspell-incorrect
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,red) :inherit unspecified))
       (t (:foreground ,red-1 :weight bold :underline t))))

     ;; full-ack
     (ack-separator ((,class (:foreground ,fg))))
     (ack-file ((,class (:foreground ,blue))))
     (ack-line ((,class (:foreground ,yellow))))
     (ack-match ((,class (:foreground ,orange :background ,bg-1 :weight bold))))

     ;; git-gutter
     (git-gutter:added ((,class (:foreground ,green :weight bold :inverse-video t))))
     (git-gutter:deleted ((,class (:foreground ,red :weight bold :inverse-video t))))
     (git-gutter:modified ((,class (:foreground ,magenta :weight bold :inverse-video t))))
     (git-gutter:unchanged ((,class (:foreground ,fg :weight bold :inverse-video t))))

     ;; git-gutter-fr
     (git-gutter-fr:added ((,class (:foreground ,green  :weight bold))))
     (git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))
     (git-gutter-fr:modified ((,class (:foreground ,magenta :weight bold))))

     ;; git-rebase-mode
     (git-rebase-hash ((,class (:foreground ,orange))))

     ;; helm
     (helm-header
      ((,class (:foreground ,green :background ,bg
                :underline nil :box nil))))
     (helm-source-header
      ((,class (:foreground ,yellow :background ,bg-1
                :underline nil :weight bold
                :box (:line-width -1 :style released-button)))))
     (helm-selection ((,class (:background ,bg+1 :underline nil))))
     (helm-selection-line ((,class (:background ,bg+1))))
     (helm-visible-mark ((,class (:foreground ,bg :background ,yellow-2))))
     (helm-candidate-number ((,class (:foreground ,green+4 :background ,bg-1))))
     (helm-separator ((,class (:foreground ,red :background ,bg))))
     (helm-time-zone-current ((,class (:foreground ,green+2 :background ,bg))))
     (helm-time-zone-home ((,class (:foreground ,red :background ,bg))))
     (helm-bookmark-addressbook ((,class (:foreground ,orange :background ,bg))))
     (helm-bookmark-directory ((,class (:foreground nil :background nil :inherit helm-ff-directory))))
     (helm-bookmark-file ((,class (:foreground nil :background nil :inherit helm-ff-file))))
     (helm-bookmark-gnus ((,class (:foreground ,magenta :background ,bg))))
     (helm-bookmark-info ((,class (:foreground ,green+2 :background ,bg))))
     (helm-bookmark-man ((,class (:foreground ,yellow :background ,bg))))
     (helm-bookmark-w3m ((,class (:foreground ,magenta :background ,bg))))
     (helm-buffer-not-saved ((,class (:foreground ,red :background ,bg))))
     (helm-buffer-process ((,class (:foreground ,cyan :background ,bg))))
     (helm-buffer-saved-out ((,class (:foreground ,fg :background ,bg))))
     (helm-buffer-size ((,class (:foreground ,fg-1 :background ,bg))))
     (helm-ff-directory ((,class (:foreground ,cyan :background ,bg :weight bold))))
     (helm-ff-file ((,class (:foreground ,fg :background ,bg :weight normal))))
     (helm-ff-executable ((,class (:foreground ,green+2 :background ,bg :weight normal))))
     (helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg :weight bold))))
     (helm-ff-symlink ((,class (:foreground ,yellow :background ,bg :weight bold))))
     (helm-ff-prefix ((,class (:foreground ,bg :background ,yellow :weight normal))))
     (helm-grep-cmd-line ((,class (:foreground ,cyan :background ,bg))))
     (helm-grep-file ((,class (:foreground ,fg :background ,bg))))
     (helm-grep-finish ((,class (:foreground ,green+2 :background ,bg))))
     (helm-grep-lineno ((,class (:foreground ,fg-1 :background ,bg))))
     (helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     (helm-grep-running ((,class (:foreground ,red :background ,bg))))
     (helm-moccur-buffer ((,class (:foreground ,cyan :background ,bg))))
     (helm-mu-contacts-address-face ((,class (:foreground ,fg-1 :background ,bg))))
     (helm-mu-contacts-name-face ((,class (:foreground ,fg :background ,bg))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((,class (:foreground ,fg :background ,bg+1))))
     (helm-swoop-target-word-face ((,class (:foreground ,yellow :background ,bg+2 :weight bold))))

     ;; hl-line-mode
     (hl-line-face ((,class (:background ,bg-05))
                    (t :weight bold)))
     (hl-line ((,class (:background ,bg-05)) ; old emacsen
               (t :weight bold)))

     ;; hl-sexp
     (hl-sexp-face ((,class (:background ,bg+1))
                    (t :weight bold)))

     ;; IDO
     (ido-first-match ((,class (:foreground ,yellow :weight bold))))
     (ido-only-match ((,class (:foreground ,orange :weight bold))))
     (ido-subdir ((,class (:foreground ,yellow))))
     (ido-indicator ((,class (:foreground ,yellow :background ,red-4))))

     ;; iedit-mode
     (iedit-occurrence ((,class (:background ,bg+2 :weight bold))))

     ;; js2-mode
     (js2-warning ((,class (:underline ,orange))))
     (js2-error ((,class (:foreground ,red :weight bold))))
     (js2-jsdoc-tag ((,class (:foreground ,green-1))))
     (js2-jsdoc-type ((,class (:foreground ,green+2))))
     (js2-jsdoc-value ((,class (:foreground ,green+3))))
     (js2-function-param ((,class (:foreground ,green+3))))
     (js2-external-variable ((,class (:foreground ,orange))))

     ;; linum-mode
     (linum ((,class (:foreground ,green+2 :background ,bg))))

     ;; magit
     (magit-item-highlight ((,class (:background ,bg+05))))
     (magit-section-title ((,class (:foreground ,yellow :weight bold))))
     (magit-process-ok ((,class (:foreground ,green :weight bold))))
     (magit-process-ng ((,class (:foreground ,red :weight bold))))
     (magit-branch ((,class (:foreground ,blue :weight bold))))
     (magit-log-author ((,class (:foreground ,orange))))
     (magit-log-sha1 ((,class (:foreground ,orange))))

     ;; mic-paren
     (paren-face-match ((,class (:foreground ,cyan :background ,bg :weight bold))))
     (paren-face-mismatch ((,class (:foreground ,bg :background ,magenta :weight bold))))
     (paren-face-no-match ((,class (:foreground ,bg :background ,red :weight bold))))

     ;; org-mode
     (org-agenda-date-today
      ((,class (:foreground ,fg+1 :slant italic :weight bold))) t)
     (org-agenda-structure
      ((,class (:inherit font-lock-comment-face))))
     (org-archived ((,class (:foreground ,fg :weight bold))))
     (org-checkbox ((,class (:background ,bg+2 :foreground ,fg+1
                             :box (:line-width 1 :style released-button)))))
     (org-date ((,class (:foreground ,blue :underline t))))
     (org-deadline-announce ((,class (:foreground ,red-1))))
     (org-done ((,class (:bold t :weight bold :foreground ,green+3))))
     (org-formula ((,class (:foreground ,yellow-2))))
     (org-headline-done ((,class (:foreground ,green+3))))
     (org-hide ((,class (:foreground ,bg-1))))
     (org-level-1 ((,class (:foreground ,orange))))
     (org-level-2 ((,class (:foreground ,green+4))))
     (org-level-3 ((,class (:foreground ,blue-1))))
     (org-level-4 ((,class (:foreground ,yellow-2))))
     (org-level-5 ((,class (:foreground ,cyan))))
     (org-level-6 ((,class (:foreground ,green+2))))
     (org-level-7 ((,class (:foreground ,red-4))))
     (org-level-8 ((,class (:foreground ,blue-4))))
     (org-link ((,class (:foreground ,yellow-2 :underline t))))
     (org-scheduled ((,class (:foreground ,green+4))))
     (org-scheduled-previously ((,class (:foreground ,red))))
     (org-scheduled-today ((,class (:foreground ,blue+1))))
     (org-sexp-date ((,class (:foreground ,blue+1 :underline t))))
     (org-special-keyword ((,class (:inherit font-lock-comment-face))))
     (org-table ((,class (:foreground ,green+2))))
     (org-tag ((,class (:bold t :weight bold))))
     (org-time-grid ((,class (:foreground ,orange))))
     (org-todo ((,class (:bold t :foreground ,red :weight bold))))
     (org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
     (org-warning ((,class (:bold t :foreground ,red :weight bold :underline nil))))
     (org-column ((,class (:background ,bg-1))))
     (org-column-title ((,class (:background ,bg-1 :underline t :weight bold))))
     (org-mode-line-clock ((,class (:foreground ,fg :background ,bg-1))))
     (org-mode-line-clock-overrun ((,class (:foreground ,bg :background ,red-1))))
     (org-ellipsis ((,class (:foreground ,yellow-1 :underline t))))
     (org-footnote ((,class (:foreground ,cyan :underline t))))

     ;; outline
     (outline-1 ((,class (:foreground ,orange))))
     (outline-2 ((,class (:foreground ,green+4))))
     (outline-3 ((,class (:foreground ,blue-1))))
     (outline-4 ((,class (:foreground ,yellow-2))))
     (outline-5 ((,class (:foreground ,cyan))))
     (outline-6 ((,class (:foreground ,green+2))))
     (outline-7 ((,class (:foreground ,red-4))))
     (outline-8 ((,class (:foreground ,blue-4))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,fg))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,green+4))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow-2))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,green+2))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,blue+1))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,yellow-1))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,green+1))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,blue-2))))
     (rainbow-delimiters-depth-10-face ((,class (:foreground ,orange))))
     (rainbow-delimiters-depth-11-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-12-face ((,class (:foreground ,blue-5))))

     ;; sh-mode
     (sh-heredoc     ((,class (:foreground ,yellow :bold t))))
     (sh-quoted-exec ((,class (:foreground ,red))))

     ;; show-paren
     (show-paren-mismatch ((,class (:foreground ,red+1 :background ,bg+3 :weight bold))))
     (show-paren-match ((,class (:background ,bg+3 :weight bold))))

     ;; smartparens
     (sp-show-pair-mismatch-face ((,class (:foreground ,red+1 :background ,bg+3 :weight bold))))
     (sp-show-pair-match-face ((,class (:background ,bg+3 :weight bold))))

     ;; sml-mode-line
     '(sml-modeline-end-face ((,class :inherit default :width condensed)))

     ;; SLIME
     (slime-repl-output-face ((,class (:foreground ,red))))
     (slime-repl-inputed-output-face ((,class (:foreground ,green))))
     (slime-error-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,red)))
       (t
        (:underline ,red))))
     (slime-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,orange)))
       (t
        (:underline ,orange))))
     (slime-style-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,yellow)))
       (t
        (:underline ,yellow))))
     (slime-note-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,green)))
       (t
        (:underline ,green))))
     (slime-highlight-face ((,class (:inherit highlight))))

     ;; speedbar
     (speedbar-button-face ((,class (:foreground ,green+2))))
     (speedbar-directory-face ((,class (:foreground ,cyan))))
     (speedbar-file-face ((,class (:foreground ,fg))))
     (speedbar-highlight-face ((,class (:foreground ,bg :background ,green+2))))
     (speedbar-selected-face ((,class (:foreground ,red))))
     (speedbar-separator-face ((,class (:foreground ,bg :background ,blue-1))))
     (speedbar-tag-face ((,class (:foreground ,yellow))))

     ;; tabbar
     (tabbar-button ((,class (:foreground ,fg :background ,bg))))
     (tabbar-selected ((,class (:foreground ,fg :background ,bg
                                :box (:line-width -1 :style pressed-button)))))
     (tabbar-unselected ((,class (:foreground ,fg :background ,bg+1
                                  :box (:line-width -1 :style released-button)))))

     ;; term
     (term-color-black ((,class (:foreground ,bg :background ,bg-1))))
     (term-color-red ((,class (:foreground ,red-2 :background ,red-4))))
     (term-color-green ((,class (:foreground ,green :background ,green+2))))
     (term-color-yellow ((,class (:foreground ,orange :background ,yellow))))
     (term-color-blue ((,class (:foreground ,blue-1 :background ,blue-4))))
     (term-color-magenta ((,class (:foreground ,magenta :background ,red))))
     (term-color-cyan ((,class (:foreground ,cyan :background ,blue))))
     (term-color-white ((,class (:foreground ,fg :background ,fg-1))))
     (term-default-fg-color ((,class (:inherit term-color-white))))
     (term-default-bg-color ((,class (:inherit term-color-black))))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,fg+1 :weight bold))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,red-1 :weight bold))))
     (undo-tree-visualizer-default-face ((,class (:foreground ,fg))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))
     (undo-tree-visualizer-unmodified-face ((,class (:foreground ,cyan))))

     ;; whitespace-mode
     (whitespace-space ((,class (:background ,bg+1 :foreground ,bg+1))))
     (whitespace-hspace ((,class (:background ,bg+1 :foreground ,bg+1))))
     (whitespace-tab ((,class (:background ,red-1))))
     (whitespace-newline ((,class (:foreground ,bg+1))))
     (whitespace-trailing ((,class (:background ,red))))
     (whitespace-line ((,class (:background ,bg :foreground ,magenta))))
     (whitespace-space-before-tab ((,class (:background ,orange :foreground ,orange))))
     (whitespace-indentation ((,class (:background ,yellow :foreground ,red))))
     (whitespace-empty ((,class (:background ,yellow))))
     (whitespace-space-after-tab ((,class (:background ,yellow :foreground ,red))))

     ;; which-func-mode
     (which-func ((,class (:foreground ,green+4))))

     ;; yascroll
     (yascroll:thumb-text-area ((,class (:background ,bg-1))))
     (yascroll:thumb-fringe ((,class (:background ,bg-1 :foreground ,bg-1))))
     )))

(defun define-zenburn-theme ()
  "Define the zenburn theme (only one variant for now)"
  (deftheme zenburn "A low contrast theme")
  (with-zenburn-colors
   (apply 'custom-theme-set-faces 'zenburn (zenburn-face-specs)))
  (provide-theme 'zenburn))

(defun set-zenburn-extra-org-statuses ()
  (require 'org)
  (zenburn-with-color-variables
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,yellow
                      :foreground ,zenburn-yellow
                      :weight bold :box nil))
           ("WAIT" . (;:background ,orange
                      :foreground ,zenburn-orange
                      :weight bold :box nil))))))

;;; Debugging functions:

(defun set-colors-zenburn ()
  "Sets the colors to the zenburn theme"
  (interactive)
  (with-zenburn-colors
    (apply 'custom-set-faces (zenburn-face-specs))))

(provide 'color-theme-zenburn)
;;; color-theme-zenburn.el ends here
