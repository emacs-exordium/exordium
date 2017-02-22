;;; color-theme-zenburn.el --- A low contrast color theme.
;;;
;;; Credits:
;;; Jani Nurminen created the original theme for vim on such this port
;;; is based.
;;; Inspired by the emacs theme by Bozhidar Batsov:
;;; https://github.com/bbatsov/zenburn-emacs

(require 'org)
(require 'init-prefs)

;;; Color palette.
;;; `+N' suffixes indicate a color is lighter.
;;; `-N' suffixes indicate a color is darker.

(defconst zenburn-colors
  '((zenburn-fg+1     . "#FFFFEF")
    (zenburn-fg       . "#DCDCCC")
    (zenburn-fg-1     . "#656555")
    (zenburn-bg-2     . "#000000")
    (zenburn-bg-1     . "#2B2B2B")
    (zenburn-bg-05    . "#383838")
    (zenburn-bg       . "#3F3F3F")
    (zenburn-bg+05    . "#494949")
    (zenburn-bg+1     . "#4F4F4F")
    (zenburn-bg+2     . "#5F5F5F")
    (zenburn-bg+3     . "#6F6F6F")
    (zenburn-red+1    . "#DCA3A3")
    (zenburn-red      . "#CC9393")
    (zenburn-red-1    . "#BC8383")
    (zenburn-red-2    . "#AC7373")
    (zenburn-red-3    . "#9C6363")
    (zenburn-red-4    . "#8C5353")
    (zenburn-orange   . "#DFAF8F")
    (zenburn-yellow   . "#F0DFAF")
    (zenburn-yellow-1 . "#E0CF9F")
    (zenburn-yellow-2 . "#D0BF8F")
    (zenburn-green-1  . "#5F7F5F")
    (zenburn-green    . "#7F9F7F")
    (zenburn-green+1  . "#8FB28F")
    (zenburn-green+2  . "#9FC59F")
    (zenburn-green+3  . "#AFD8AF")
    (zenburn-green+4  . "#BFEBBF")
    (zenburn-cyan     . "#93E0E3")
    (zenburn-blue+1   . "#94BFF3")
    (zenburn-blue     . "#8CD0D3")
    (zenburn-blue-1   . "#7CB8BB")
    (zenburn-blue-2   . "#6CA0A3")
    (zenburn-blue-3   . "#5C888B")
    (zenburn-blue-4   . "#4C7073")
    (zenburn-blue-5   . "#366060")
    (zenburn-magenta  . "#DC8CC3")))

;;; Theme definition

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
     (button ((t (:underline t))))
     (link ((t (:foreground ,zenburn-yellow :underline t :weight bold))))
     (link-visited ((t (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
     (default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
     (escape-glyph ((t (:foreground ,zenburn-yellow :bold t))))
     (fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
     (header-line ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1
                            :box (:line-width -1 :style released-button)))))
     (highlight ((t (:background ,zenburn-bg-05))))
     (success ((t (:foreground ,zenburn-green :weight bold))))
     (warning ((t (:foreground ,zenburn-orange :weight bold))))
     (shadow ((t (:background ,zenburn-bg+1))))

     ;; compilation
     (compilation-column-face ((t (:foreground ,zenburn-yellow))))
     (compilation-enter-directory-face ((t (:foreground ,zenburn-green))))
     (compilation-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (compilation-face ((t (:foreground ,zenburn-fg))))
     (compilation-info-face ((t (:foreground ,zenburn-blue))))
     (compilation-info ((t (:foreground ,zenburn-green+4 :underline t))))
     (compilation-leave-directory-face ((t (:foreground ,zenburn-green))))
     (compilation-line-face ((t (:foreground ,zenburn-yellow))))
     (compilation-line-number ((t (:foreground ,zenburn-yellow))))
     (compilation-message-face ((t (:foreground ,zenburn-blue))))
     (compilation-warning-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))
     (compilation-mode-line-exit ((t (:foreground ,zenburn-green+2 :weight bold))))
     (compilation-mode-line-fail ((t (:foreground ,zenburn-red :weight bold))))
     (compilation-mode-line-run ((t (:foreground ,zenburn-yellow :weight bold))))

     ;; completions
     (completions-annotations ((t (:foreground ,zenburn-fg-1))))

     ;; grep
     (grep-context-face ((t (:foreground ,zenburn-fg))))
     (grep-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,zenburn-blue))))
     (grep-match-face ((t (:foreground ,zenburn-orange :weight bold))))
     (match ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))

     ;; isearch
     (isearch ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
     (isearch-fail ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (lazy-highlight ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,zenburn-yellow-2 :background ,zenburn-bg :inverse-video t))))
     (hi-pink ((t (:foreground ,zenburn-magenta :background ,zenburn-bg :inverse-video t))))
     (hi-green ((t (:foreground ,zenburn-green :background ,zenburn-bg :inverse-video t))))
     (hi-blue ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :inverse-video t))))

     ;; UI
     (menu ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
     (mode-line
      ((,class (:foreground ,zenburn-green+1 :background ,zenburn-bg-1
                :box (:line-width -1 :style released-button)))
       (t :inverse-video t)))
     (mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
     (mode-line-inactive
      ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg-1 ; for powerline (previously zenburn-bg-05)
                            :box (:line-width -1 :style released-button)))))
     (region ((,class (:background ,zenburn-bg-1))
              (t :inverse-video t)))
     (secondary-selection ((t (:background ,zenburn-bg+2))))
     (trailing-whitespace ((t (:background ,zenburn-red))))
     (vertical-border ((t (:foreground ,zenburn-fg))))

     ;; font lock
     (font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))
     (font-lock-comment-face ((t (:foreground ,zenburn-green :italic nil))))
     (font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-1))))
     (font-lock-constant-face ((t (:foreground ,zenburn-green+4))))
     (font-lock-doc-face ((t (:foreground ,zenburn-green+2))))
     (font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))
     (font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))
     (font-lock-string-face ((t (:foreground ,zenburn-red))))
     (font-lock-type-face ((t (:foreground ,zenburn-blue-1))))
     (font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))
     (font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))

     (c-annotation-face ((t (:inherit font-lock-constant-face))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,zenburn-bg-1))))
     (exordium-powerline-active2 ((t (:background ,zenburn-bg-05))))
     (exordium-powerline-active3 ((t (:background ,zenburn-yellow :foreground ,zenburn-bg))))
     (exordium-powerline-active4 ((t (:background ,zenburn-red :foreground ,zenburn-bg))))
     (exordium-powerline-active5 ((t (:background ,zenburn-green :foreground ,zenburn-bg))))
     (exordium-powerline-inactive1 ((t (:background ,zenburn-bg-1))))
     (exordium-powerline-inactive2 ((t (:background ,zenburn-bg-05))))
     (exordium-powerline-inactive3 ((t (:background ,zenburn-bg :foreground ,zenburn-yellow))))
     (exordium-project-name ((t (:foreground ,zenburn-yellow))))

     (powerline-active1 ((t (:background ,zenburn-bg-05 :inherit mode-line))))
     (powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line))))
     (powerline-inactive1 ((t (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
     (powerline-inactive2 ((t (:background ,zenburn-bg+3 :inherit mode-line-inactive))))

     ;; auctex
     (font-latex-bold-face ((t (:inherit bold))))
     (font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
     (font-latex-sectioning-5-face ((t (:foreground ,zenburn-red :weight bold ))))
     (font-latex-sedate-face ((t (:foreground ,zenburn-yellow))))
     (font-latex-italic-face ((t (:foreground ,zenburn-cyan :slant italic))))
     (font-latex-string-face ((t (:inherit ,font-lock-string-face))))
     (font-latex-math-face ((t (:foreground ,zenburn-orange))))

     ;; auto-complete
     (ac-candidate-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
     (ac-selection-face ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
     (popup-tip-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
     (popup-scroll-bar-foreground-face ((t (:background ,zenburn-blue-5))))
     (popup-scroll-bar-background-face ((t (:background ,zenburn-bg-1))))
     (popup-isearch-match ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))

     ;; company
     (company-echo-common ((t (:background ,zenburn-bg+3 :foreground ,zenburn-red-4))))
     (company-preview ((t (:background ,zenburn-blue-5 :foreground ,zenburn-yellow-2))))
     (company-preview-common ((t (:background ,zenburn-blue-5 :foreground ,zenburn-yellow-1))))
     (company-preview-search ((t (:background ,zenburn-blue-4 :foreground ,zenburn-yellow))))
     (company-tooltip ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
     (company-tooltip-annotation ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-red-4))))
     (company-tooltip-annotation-selection ((t (:background ,zenburn-blue-4 :foreground ,zenburn-red-1))))
     (company-tooltip-common ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg))))
     (company-tooltip-common-selection ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg+1))))
     (company-tooltip-mouse ((t (:background ,zenburn-blue-5 :foreground ,zenburn-fg+1))))
     (company-tooltip-selection ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
     (company-scrollbar-fg ((t (:background ,zenburn-blue-5))))
     (company-scrollbar-bg ((t (:background ,zenburn-bg-1))))

     ;; diff
     (diff-added ((,class (:foreground ,zenburn-green+4 :background nil))
                  (t (:foreground ,zenburn-green-1 :background nil))))
     (diff-changed ((t (:foreground ,zenburn-yellow))))
     (diff-removed ((,class (:foreground ,zenburn-red :background nil))
                    (t (:foreground ,zenburn-red-3 :background nil))))
     (diff-refine-added ((t (:inherit diff-added :weight bold))))
     (diff-refine-change ((t (:inherit diff-changed :weight bold))))
     (diff-refine-removed ((t (:inherit diff-removed :weight bold))))
     (diff-header ((,class (:background ,zenburn-bg+2))
                   (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
     (diff-file-header
      ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :bold t))
       (t (:background ,zenburn-fg :foreground ,zenburn-bg :bold t))))


     ;; dired+
     (diredp-display-msg ((t (:foreground ,zenburn-blue))))
     (diredp-compressed-file-suffix ((t (:foreground ,zenburn-orange))))
     (diredp-date-time ((t (:foreground ,zenburn-magenta))))
     (diredp-deletion ((t (:foreground ,zenburn-yellow))))
     (diredp-deletion-file-name ((t (:foreground ,zenburn-red))))
     (diredp-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
     (diredp-dir-priv ((t (:foreground ,zenburn-cyan))))
     (diredp-exec-priv ((t (:foreground ,zenburn-red))))
     (diredp-executable-tag ((t (:foreground ,zenburn-green+1))))
     (diredp-file-name ((t (:foreground ,zenburn-blue))))
     (diredp-file-suffix ((t (:foreground ,zenburn-green))))
     (diredp-flag-mark ((t (:foreground ,zenburn-yellow))))
     (diredp-flag-mark-line ((t (:foreground ,zenburn-orange))))
     (diredp-ignored-file-name ((t (:foreground ,zenburn-red))))
     (diredp-link-priv ((t (:foreground ,zenburn-yellow))))
     (diredp-mode-line-flagged ((t (:foreground ,zenburn-yellow))))
     (diredp-mode-line-marked ((t (:foreground ,zenburn-orange))))
     (diredp-no-priv ((t (:foreground ,zenburn-fg))))
     (diredp-number ((t (:foreground ,zenburn-green+1))))
     (diredp-other-priv ((t (:foreground ,zenburn-yellow-1))))
     (diredp-rare-priv ((t (:foreground ,zenburn-red-1))))
     (diredp-read-priv ((t (:foreground ,zenburn-green-1))))
     (diredp-symlink ((t (:foreground ,zenburn-yellow))))
     (diredp-write-priv ((t (:foreground ,zenburn-magenta))))

     ;; ediff
     (ediff-current-diff-A ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (ediff-current-diff-Ancestor ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (ediff-current-diff-B ((t (:foreground ,zenburn-fg :background ,zenburn-green-1))))
     (ediff-current-diff-C ((t (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
     (ediff-even-diff-A ((t (:background ,zenburn-bg+1))))
     (ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+1))))
     (ediff-even-diff-B ((t (:background ,zenburn-bg+1))))
     (ediff-even-diff-C ((t (:background ,zenburn-bg+1))))
     (ediff-fine-diff-A ((t (:foreground ,zenburn-fg :background ,zenburn-red-2 :weight bold))))
     (ediff-fine-diff-Ancestor ((t (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
     (ediff-fine-diff-B ((t (:foreground ,zenburn-fg :background ,zenburn-green :weight bold))))
     (ediff-fine-diff-C ((t (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
     (ediff-odd-diff-A ((t (:background ,zenburn-bg+2))))
     (ediff-odd-diff-Ancestor ((t (:background ,zenburn-bg+2))))
     (ediff-odd-diff-B ((t (:background ,zenburn-bg+2))))
     (ediff-odd-diff-C ((t (:background ,zenburn-bg+2))))

     ;; eshell
     (eshell-prompt ((t (:foreground ,zenburn-yellow :weight bold))))
     (eshell-ls-archive ((t (:foreground ,zenburn-red-1 :weight bold))))
     (eshell-ls-backup ((t (:inherit font-lock-comment-face))))
     (eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
     (eshell-ls-directory ((t (:foreground ,zenburn-blue+1 :weight bold))))
     (eshell-ls-executable ((t (:foreground ,zenburn-red+1 :weight bold))))
     (eshell-ls-unreadable ((t (:foreground ,zenburn-fg))))
     (eshell-ls-missing ((t (:inherit font-lock-warning-face))))
     (eshell-ls-product ((t (:inherit font-lock-doc-face))))
     (eshell-ls-special ((t (:foreground ,zenburn-yellow :weight bold))))
     (eshell-ls-symlink ((t (:foreground ,zenburn-cyan :weight bold))))

     ;; flycheck
     (flycheck-error
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))
       (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (flycheck-warning
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))
       (t (:foreground ,zenburn-yellow :weight bold :underline t))))
     (flycheck-info
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))
       (t (:foreground ,zenburn-cyan :weight bold :underline t))))
     (flycheck-fringe-error ((t (:foreground ,zenburn-red-1 :weight bold))))
     (flycheck-fringe-warning ((t (:foreground ,zenburn-yellow :weight bold))))
     (flycheck-fringe-info ((t (:foreground ,zenburn-cyan :weight bold))))

     ;; flymake
     (flymake-errline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-red)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (flymake-warnline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-orange)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,zenburn-orange :weight bold :underline t))))
     (flymake-infoline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-green)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,zenburn-green-1 :weight bold :underline t))))

     ;; Rtags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,zenburn-red :foreground ,zenburn-bg)
                                `(:underline (:color ,zenburn-red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,zenburn-orange :foreground ,zenburn-bg)
                                 `(:underline (:color ,zenburn-orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,zenburn-green :foreground ,zenburn-bg)
                                  `(:underline (:color ,zenburn-green :style wave))))))
     (rtags-skippedline ((t (:background ,zenburn-bg+1))))

     ;; flyspell
     (flyspell-duplicate
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))
       (t (:foreground ,zenburn-orange :weight bold :underline t))))
     (flyspell-incorrect
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-red) :inherit unspecified))
       (t (:foreground ,zenburn-red-1 :weight bold :underline t))))

     ;; full-ack
     (ack-separator ((t (:foreground ,zenburn-fg))))
     (ack-file ((t (:foreground ,zenburn-blue))))
     (ack-line ((t (:foreground ,zenburn-yellow))))
     (ack-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))

     ;; git-commit
     (git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
     (git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
     (git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))

     ;; git-gutter
     (git-gutter:added ((t (:foreground ,zenburn-green :weight bold :inverse-video t))))
     (git-gutter:deleted ((t (:foreground ,zenburn-red :weight bold :inverse-video t))))
     (git-gutter:modified ((t (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
     (git-gutter:unchanged ((t (:foreground ,zenburn-fg :weight bold :inverse-video t))))

     ;; git-gutter-fr
     (git-gutter-fr:added ((t (:foreground ,zenburn-green  :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,zenburn-red :weight bold))))
     (git-gutter-fr:modified ((t (:foreground ,zenburn-magenta :weight bold))))

     ;; git-rebase
     (git-rebase-hash ((t (:foreground ,zenburn-orange))))

     ;; helm
     (helm-header
      ((t (:foreground ,zenburn-green :background ,zenburn-bg
                :underline nil :box nil))))
     (helm-source-header
      ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1
                :underline nil :weight bold
                :box (:line-width -1 :style released-button)))))
     (helm-selection ((t (:background ,zenburn-bg+1 :underline nil))))
     (helm-selection-line ((t (:background ,zenburn-bg+1))))
     (helm-visible-mark ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
     (helm-candidate-number ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
     (helm-separator ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-time-zone-current ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-time-zone-home ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-bookmark-addressbook ((t (:foreground ,zenburn-orange :background ,zenburn-bg))))
     (helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
     (helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
     (helm-bookmark-gnus ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
     (helm-bookmark-info ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-bookmark-man ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
     (helm-bookmark-w3m ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
     (helm-buffer-not-saved ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-buffer-process ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-buffer-saved-out ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (helm-buffer-size ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-ff-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
     (helm-ff-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
     (helm-ff-executable ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
     (helm-ff-invalid-symlink ((t (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
     (helm-ff-symlink ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
     (helm-ff-prefix ((t (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
     (helm-grep-cmd-line ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-grep-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (helm-grep-finish ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-grep-lineno ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
     (helm-grep-running ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-moccur-buffer ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-mu-contacts-address-face ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-mu-contacts-name-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
     (helm-swoop-target-word-face ((t (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))))

     ;; hl-line-mode
     (hl-line-face ((,class (:background ,zenburn-bg-05))
                    (t :weight bold)))
     (hl-line ((,class (:background ,zenburn-bg-05)) ; old emacsen
               (t :weight bold)))

     ;; hl-sexp
     (hl-sexp-face ((,class (:background ,zenburn-bg+1))
                    (t :weight bold)))

     ;; IDO
     (ido-first-match ((t (:foreground ,zenburn-yellow :weight bold))))
     (ido-only-match ((t (:foreground ,zenburn-orange :weight bold))))
     (ido-subdir ((t (:foreground ,zenburn-yellow))))
     (ido-indicator ((t (:foreground ,zenburn-yellow :background ,zenburn-red-4))))

     ;; iedit-mode
     (iedit-occurrence ((t (:background ,zenburn-bg+2 :weight bold))))

     ;; js2-mode
     (js2-warning ((t (:underline ,zenburn-orange :style wave))))
     (js2-error ((t (:underline ,zenburn-red :style wave))))
     (js2-jsdoc-tag ((t (:foreground ,zenburn-green-1))))
     (js2-jsdoc-type ((t (:foreground ,zenburn-green+2))))
     (js2-jsdoc-value ((t (:foreground ,zenburn-green+3))))
     (js2-function-param ((t (:foreground ,zenburn-green+3))))
     (js2-external-variable ((t (:foreground ,zenburn-orange))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,zenburn-orange :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,zenburn-red :style wave)))))

     ;; linum-mode
     (linum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))


;;;;; magit
;;;;;; headings and diffs
     (magit-section-highlight           ((t (:background ,zenburn-bg+05))))
     (magit-section-heading             ((t (:foreground ,zenburn-yellow :weight bold))))
     (magit-section-heading-selection   ((t (:foreground ,zenburn-orange :weight bold))))
     (magit-diff-file-heading           ((t (:weight bold))))
     (magit-diff-file-heading-highlight ((t (:background ,zenburn-bg+05  :weight bold))))
     (magit-diff-file-heading-selection ((t (:background ,zenburn-bg+05
                                                          :foreground ,zenburn-orange :weight bold))))
     (magit-diff-hunk-heading           ((t (:background ,zenburn-bg+1))))
     (magit-diff-hunk-heading-highlight ((t (:background ,zenburn-bg+2))))
     (magit-diff-hunk-heading-selection ((t (:background ,zenburn-bg+2
                                                          :foreground ,zenburn-orange))))
     (magit-diff-lines-heading          ((t (:background ,zenburn-orange
                                                          :foreground ,zenburn-bg+2))))
     (magit-diff-context-highlight      ((t (:background ,zenburn-bg+05
                                                          :foreground "grey70"))))
     (magit-diffstat-added   ((t (:foreground ,zenburn-green+4))))
     (magit-diffstat-removed ((t (:foreground ,zenburn-red))))
;;;;;; popup
     (magit-popup-heading             ((t (:foreground ,zenburn-yellow  :weight bold))))
     (magit-popup-key                 ((t (:foreground ,zenburn-green-1 :weight bold))))
     (magit-popup-argument            ((t (:foreground ,zenburn-green   :weight bold))))
     (magit-popup-disabled-argument   ((t (:foreground ,zenburn-fg-1    :weight normal))))
     (magit-popup-option-value        ((t (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
     (magit-process-ok    ((t (:foreground ,zenburn-green  :weight bold))))
     (magit-process-ng    ((t (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
     (magit-log-author    ((t (:foreground ,zenburn-orange))))
     (magit-log-date      ((t (:foreground ,zenburn-fg-1))))
     (magit-log-graph     ((t (:foreground ,zenburn-fg+1))))
;;;;;; sequence
     (magit-sequence-pick ((t (:foreground ,zenburn-yellow-2))))
     (magit-sequence-stop ((t (:foreground ,zenburn-green))))
     (magit-sequence-part ((t (:foreground ,zenburn-yellow))))
     (magit-sequence-head ((t (:foreground ,zenburn-blue))))
     (magit-sequence-drop ((t (:foreground ,zenburn-red))))
     (magit-sequence-done ((t (:foreground ,zenburn-fg-1))))
     (magit-sequence-onto ((t (:foreground ,zenburn-fg-1))))
;;;;;; bisect
     (magit-bisect-good ((t (:foreground ,zenburn-green))))
     (magit-bisect-skip ((t (:foreground ,zenburn-yellow))))
     (magit-bisect-bad  ((t (:foreground ,zenburn-red))))
;;;;;; blame
     (magit-blame-heading ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
     (magit-blame-hash    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
     (magit-blame-name    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
     (magit-blame-date    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
     (magit-blame-summary ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                            :weight bold))))
;;;;;; references etc
     (magit-dimmed         ((t (:foreground ,zenburn-bg+3))))
     (magit-hash           ((t (:foreground ,zenburn-bg+3))))
     (magit-tag            ((t (:foreground ,zenburn-orange :weight bold))))
     (magit-branch-remote  ((t (:foreground ,zenburn-green  :weight bold))))
     (magit-branch-local   ((t (:foreground ,zenburn-blue   :weight bold))))
     (magit-branch-current ((t (:foreground ,zenburn-blue   :weight bold :box t))))
     (magit-head           ((t (:foreground ,zenburn-blue   :weight bold))))
     (magit-refname        ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
     (magit-refname-stash  ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
     (magit-refname-wip    ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
     (magit-signature-good      ((t (:foreground ,zenburn-green))))
     (magit-signature-bad       ((t (:foreground ,zenburn-red))))
     (magit-signature-untrusted ((t (:foreground ,zenburn-yellow))))
     (magit-cherry-unmatched    ((t (:foreground ,zenburn-cyan))))
     (magit-cherry-equivalent   ((t (:foreground ,zenburn-magenta))))
     (magit-reflog-commit       ((t (:foreground ,zenburn-green))))
     (magit-reflog-amend        ((t (:foreground ,zenburn-magenta))))
     (magit-reflog-merge        ((t (:foreground ,zenburn-green))))
     (magit-reflog-checkout     ((t (:foreground ,zenburn-blue))))
     (magit-reflog-reset        ((t (:foreground ,zenburn-red))))
     (magit-reflog-rebase       ((t (:foreground ,zenburn-magenta))))
     (magit-reflog-cherry-pick  ((t (:foreground ,zenburn-green))))
     (magit-reflog-remote       ((t (:foreground ,zenburn-cyan))))
     (magit-reflog-other        ((t (:foreground ,zenburn-cyan))))

     ;; mic-paren
     (paren-face-match ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
     (paren-face-mismatch ((t (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))
     (paren-face-no-match ((t (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))))

     ;; org-mode
     (org-agenda-date-today
      ((t (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
     (org-agenda-structure
      ((t (:inherit font-lock-comment-face))))
     (org-archived ((t (:foreground ,zenburn-fg :weight bold))))
     (org-checkbox ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                             :box (:line-width 1 :style released-button)))))
     (org-date ((t (:foreground ,zenburn-blue :underline t))))
     (org-deadline-announce ((t (:foreground ,zenburn-red-1))))
     (org-done ((t (:bold t :weight bold :foreground ,zenburn-green+3))))
     (org-formula ((t (:foreground ,zenburn-yellow-2))))
     (org-headline-done ((t (:foreground ,zenburn-green+3))))
     (org-hide ((t (:foreground ,zenburn-bg-1))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,zenburn-green+2)
                                    (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-level-1 ((t
                    ,(append `(:foreground ,zenburn-orange)
                             (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-level-2 ((t (:foreground ,zenburn-green+4))))
     (org-level-3 ((t (:foreground ,zenburn-blue-1))))
     (org-level-4 ((t (:foreground ,zenburn-yellow-2))))
     (org-level-5 ((t (:foreground ,zenburn-cyan))))
     (org-level-6 ((t (:foreground ,zenburn-green+2))))
     (org-level-7 ((t (:foreground ,zenburn-red-4))))
     (org-level-8 ((t (:foreground ,zenburn-blue-4))))
     (org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))
     (org-scheduled ((t (:foreground ,zenburn-green+4))))
     (org-scheduled-previously ((t (:foreground ,zenburn-red))))
     (org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
     (org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
     (org-special-keyword ((t (:inherit font-lock-comment-face))))
     (org-table ((t (:foreground ,zenburn-green+2))))
     (org-tag ((t (:bold t :weight bold))))
     (org-time-grid ((t (:foreground ,zenburn-orange))))
     (org-todo ((t (:bold t :foreground ,zenburn-red :weight bold))))
     (org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
     (org-warning ((t (:bold t :foreground ,zenburn-red :weight bold :underline nil))))
     (org-column ((t (:background ,zenburn-bg-1))))
     (org-column-title ((t (:background ,zenburn-bg-1 :underline t :weight bold))))
     (org-mode-line-clock ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
     (org-mode-line-clock-overrun ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))
     (org-ellipsis ((t (:foreground ,zenburn-yellow-1 :underline t))))
     (org-footnote ((t (:foreground ,zenburn-cyan :underline t))))

     ;; outline
     (outline-1 ((t (:foreground ,zenburn-orange))))
     (outline-2 ((t (:foreground ,zenburn-green+4))))
     (outline-3 ((t (:foreground ,zenburn-blue-1))))
     (outline-4 ((t (:foreground ,zenburn-yellow-2))))
     (outline-5 ((t (:foreground ,zenburn-cyan))))
     (outline-6 ((t (:foreground ,zenburn-green+2))))
     (outline-7 ((t (:foreground ,zenburn-red-4))))
     (outline-8 ((t (:foreground ,zenburn-blue-4))))

     ;; Clojure
     (clojure-test-failure-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))
     (clojure-test-error-face ((t (:foreground ,zenburn-red :weight bold :underline t))))
     (clojure-test-success-face ((t (:foreground ,zenburn-green+1 :weight bold :underline t))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-fg))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-green+4))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-yellow-2))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-cyan))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-green+2))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-blue+1))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-yellow-1))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-green+1))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-blue-2))))
     (rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-orange))))
     (rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-green))))
     (rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-blue-5))))

     ;; sh-mode
     (sh-hezenburn-redoc     ((t (:foreground ,zenburn-yellow :bold t))))
     (sh-quoted-exec ((t (:foreground ,zenburn-red))))

     ;; show-paren
     (show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
     (show-paren-match ((t (:background ,zenburn-bg+3 :weight bold))))

     ;; smartparens
     (sp-show-pair-mismatch-face ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
     (sp-show-pair-match-face ((t (:background ,zenburn-bg+3 :weight bold))))

     ;; sml-mode-line
     '(sml-modeline-end-face ((t :inherit default :width condensed)))

     ;; SLIME
     (slime-repl-output-face ((t (:foreground ,zenburn-red))))
     (slime-repl-inputed-output-face ((t (:foreground ,zenburn-green))))
     (slime-error-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-red)))
       (t
        (:underline ,zenburn-red))))
     (slime-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-orange)))
       (t
        (:underline ,zenburn-orange))))
     (slime-style-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-yellow)))
       (t
        (:underline ,zenburn-yellow))))
     (slime-note-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,zenburn-green)))
       (t
        (:underline ,zenburn-green))))
     (slime-highlight-face ((t (:inherit highlight))))

     ;; Emacs lisp
     (eval-sexp-fu-flash ((t (:background ,zenburn-orange :foreground ,zenburn-bg))))
     (eval-sexp-fu-flash-error ((t (:background ,zenburn-red :foreground ,zenburn-bg))))

     ;; speedbar
     (speedbar-button-face ((t (:foreground ,zenburn-green+2))))
     (speedbar-directory-face ((t (:foreground ,zenburn-cyan))))
     (speedbar-file-face ((t (:foreground ,zenburn-fg))))
     (speedbar-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+2))))
     (speedbar-selected-face ((t (:foreground ,zenburn-red))))
     (speedbar-separator-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
     (speedbar-tag-face ((t (:foreground ,zenburn-yellow))))

     ;; tabbar
     (tabbar-button ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (tabbar-selected ((t (:foreground ,zenburn-fg :background ,zenburn-bg
                                :box (:line-width -1 :style pressed-button)))))
     (tabbar-unselected ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1
                                  :box (:line-width -1 :style released-button)))))

     ;; term
     (term-color-black ((t (:foreground ,zenburn-bg :background ,zenburn-bg-1))))
     (term-color-red ((t (:foreground ,zenburn-red-2 :background ,zenburn-red-4))))
     (term-color-green ((t (:foreground ,zenburn-green :background ,zenburn-green+2))))
     (term-color-yellow ((t (:foreground ,zenburn-orange :background ,zenburn-yellow))))
     (term-color-blue ((t (:foreground ,zenburn-blue-1 :background ,zenburn-blue-4))))
     (term-color-magenta ((t (:foreground ,zenburn-magenta :background ,zenburn-red))))
     (term-color-cyan ((t (:foreground ,zenburn-cyan :background ,zenburn-blue))))
     (term-color-white ((t (:foreground ,zenburn-fg :background ,zenburn-fg-1))))
     (term-default-fg-color ((t (:inherit term-color-white))))
     (term-default-bg-color ((t (:inherit term-color-black))))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburn-fg+1 :weight bold))))
     (undo-tree-visualizer-current-face ((t (:foreground ,zenburn-red-1 :weight bold))))
     (undo-tree-visualizer-default-face ((t (:foreground ,zenburn-fg))))
     (undo-tree-visualizer-register-face ((t (:foreground ,zenburn-yellow))))
     (undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburn-cyan))))

     ;; whitespace-mode
     (whitespace-space ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
     (whitespace-hspace ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
     (whitespace-tab ((t (:background ,zenburn-red-1))))
     (whitespace-newline ((t (:foreground ,zenburn-bg+1))))
     (whitespace-trailing ((t (:background ,zenburn-red))))
     (whitespace-line ((t (:background ,zenburn-bg :foreground ,zenburn-magenta))))
     (whitespace-space-before-tab ((t (:background ,zenburn-orange :foreground ,zenburn-orange))))
     (whitespace-indentation ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
     (whitespace-empty ((t (:background ,zenburn-yellow))))
     (whitespace-space-after-tab ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))

     ;; which-func-mode
     (which-func ((t (:foreground ,zenburn-bg-1))))

     ;; yascroll
     (yascroll:thumb-text-area ((t (:background ,zenburn-bg-1))))
     (yascroll:thumb-fringe ((t (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
     )))


(defmacro zenburn-variables ()
  "Return a backquote which defines a list of variables.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(
     (ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow
                                         ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg])
;;;;; fill-column-indicator
     (fci-rule-color ,zenburn-bg+05)
;;;;; vc-annotate
     (vc-annotate-color-map
      '(( 20. . ,zenburn-red-1)
        ( 40. . ,zenburn-red)
        ( 60. . ,zenburn-orange)
        ( 80. . ,zenburn-yellow-2)
        (100. . ,zenburn-yellow-1)
        (120. . ,zenburn-yellow)
        (140. . ,zenburn-green-1)
        (160. . ,zenburn-green)
        (180. . ,zenburn-green+1)
        (200. . ,zenburn-green+2)
        (220. . ,zenburn-green+3)
        (240. . ,zenburn-green+4)
        (260. . ,zenburn-cyan)
        (280. . ,zenburn-blue-2)
        (300. . ,zenburn-blue-1)
        (320. . ,zenburn-blue)
        (340. . ,zenburn-blue+1)
        (360. . ,zenburn-magenta)))
     (vc-annotate-very-old-color ,zenburn-magenta)
     (vc-annotate-background ,zenburn-bg-1)
     )))

(defun define-zenburn-theme ()
  "Define the zenburn theme (only one variant for now)"
  (deftheme zenburn "A low contrast theme")
  (with-zenburn-colors
   (apply 'custom-theme-set-faces 'zenburn (zenburn-face-specs)))
  (with-zenburn-colors
   (apply 'custom-theme-set-variables 'zenburn (zenburn-variables)))
  (provide-theme 'zenburn))

;;; Extra functions

(defun set-zenburn-extra-org-statuses ()
  (require 'org)
  (with-zenburn-colors
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,zenburn-yellow
                      :foreground ,zenburn-yellow
                      :weight bold :box nil))
           ("WAIT" . (;:background ,zenburn-orange
                      :foreground ,zenburn-orange
                      :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-zenburn ()
  "Sets the colors to the zenburn theme"
  (interactive)
  (with-zenburn-colors
    (apply 'custom-set-faces (zenburn-face-specs))))

(provide 'color-theme-zenburn)

;;; color-theme-zenburn.el ends here
