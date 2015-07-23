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
     (button ((,class (:underline t))))
     (link ((,class (:foreground ,zenburn-yellow :underline t :weight bold))))
     (link-visited ((,class (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
     (default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (cursor ((,class (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
     (escape-glyph ((,class (:foreground ,zenburn-yellow :bold t))))
     (fringe ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
     (header-line ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg-1
                            :box (:line-width -1 :style released-button)))))
     (highlight ((,class (:background ,zenburn-bg-05))))
     (success ((,class (:foreground ,zenburn-green :weight bold))))
     (warning ((,class (:foreground ,zenburn-orange :weight bold))))
     (shadow ((,class (:background ,zenburn-bg+1))))

     ;; compilation
     (compilation-column-face ((,class (:foreground ,zenburn-yellow))))
     (compilation-enter-directory-face ((,class (:foreground ,zenburn-green))))
     (compilation-error-face ((,class (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (compilation-face ((,class (:foreground ,zenburn-fg))))
     (compilation-info-face ((,class (:foreground ,zenburn-blue))))
     (compilation-info ((,class (:foreground ,zenburn-green+4 :underline t))))
     (compilation-leave-directory-face ((,class (:foreground ,zenburn-green))))
     (compilation-line-face ((,class (:foreground ,zenburn-yellow))))
     (compilation-line-number ((,class (:foreground ,zenburn-yellow))))
     (compilation-message-face ((,class (:foreground ,zenburn-blue))))
     (compilation-warning-face ((,class (:foreground ,zenburn-orange :weight bold :underline t))))
     (compilation-mode-line-exit ((,class (:foreground ,zenburn-green+2 :weight bold))))
     (compilation-mode-line-fail ((,class (:foreground ,zenburn-red :weight bold))))
     (compilation-mode-line-run ((,class (:foreground ,zenburn-yellow :weight bold))))

     ;; completions
     (completions-annotations ((,class (:foreground ,zenburn-fg-1))))

     ;; grep
     (grep-context-face ((,class (:foreground ,zenburn-fg))))
     (grep-error-face ((,class (:foreground ,zenburn-red-1 :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,zenburn-blue))))
     (grep-match-face ((,class (:foreground ,zenburn-orange :weight bold))))
     (match ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))

     ;; isearch
     (isearch ((,class (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
     (isearch-fail ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (lazy-highlight ((,class (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

     ;; UI
     (menu ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (minibuffer-prompt ((,class (:foreground ,zenburn-yellow))))
     (mode-line
      ((,class (:foreground ,zenburn-green+1 :background ,zenburn-bg-1
                :box (:line-width -1 :style released-button)))
       (t :inverse-video t)))
     (mode-line-buffer-id ((,class (:foreground ,zenburn-yellow :weight bold))))
     (mode-line-inactive
      ((,class (:foreground ,zenburn-green-1 :background ,zenburn-bg-1 ; for powerline (previously zenburn-bg-05)
                            :box (:line-width -1 :style released-button)))))
     (region ((,class (:background ,zenburn-bg-1))
              (t :inverse-video t)))
     (secondary-selection ((,class (:background ,zenburn-bg+2))))
     (trailing-whitespace ((,class (:background ,zenburn-red))))
     (vertical-border ((,class (:foreground ,zenburn-fg))))

     ;; font lock
     (font-lock-builtin-face ((,class (:foreground ,zenburn-fg :weight bold))))
     (font-lock-comment-face ((,class (:foreground ,zenburn-green :italic nil))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,zenburn-green-1))))
     (font-lock-constant-face ((,class (:foreground ,zenburn-green+4))))
     (font-lock-doc-face ((,class (:foreground ,zenburn-green+2))))
     (font-lock-function-name-face ((,class (:foreground ,zenburn-cyan))))
     (font-lock-keyword-face ((,class (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-negation-char-face ((,class (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-preprocessor-face ((,class (:foreground ,zenburn-blue+1))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,zenburn-yellow :weight bold))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,zenburn-green :weight bold))))
     (font-lock-string-face ((,class (:foreground ,zenburn-red))))
     (font-lock-type-face ((,class (:foreground ,zenburn-blue-1))))
     (font-lock-variable-name-face ((,class (:foreground ,zenburn-orange))))
     (font-lock-warning-face ((,class (:foreground ,zenburn-yellow-2 :weight bold))))

     (c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;; Powerline
     (exordium-powerline-active1 ((,class (:background ,zenburn-bg-1))))
     (exordium-powerline-active2 ((,class (:background ,zenburn-bg-05))))
     (exordium-powerline-active3 ((,class (:background ,zenburn-yellow :foreground ,zenburn-bg))))
     (exordium-powerline-active4 ((,class (:background ,zenburn-red :foreground ,zenburn-bg))))
     (exordium-powerline-active5 ((,class (:background ,zenburn-green :foreground ,zenburn-bg))))
     (exordium-powerline-inactive1 ((,class (:background ,zenburn-bg-1))))
     (exordium-powerline-inactive2 ((,class (:background ,zenburn-bg-05))))
     (exordium-powerline-inactive3 ((,class (:background ,zenburn-bg :foreground ,zenburn-yellow))))
     (exordium-project-name ((,class (:foreground ,zenburn-yellow))))

     (powerline-active1 ((,class (:background ,zenburn-bg-05 :inherit mode-line))))
     (powerline-active2 ((,class (:background ,zenburn-bg+2 :inherit mode-line))))
     (powerline-inactive1 ((,class (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
     (powerline-inactive2 ((,class (:background ,zenburn-bg+3 :inherit mode-line-inactive))))

     ;; auctex
     (font-latex-bold-face ((,class (:inherit bold))))
     (font-latex-warning-face ((,class (:foreground nil :inherit font-lock-warning-face))))
     (font-latex-sectioning-5-face ((,class (:foreground ,zenburn-red :weight bold ))))
     (font-latex-sedate-face ((,class (:foreground ,zenburn-yellow))))
     (font-latex-italic-face ((,class (:foreground ,zenburn-cyan :slant italic))))
     (font-latex-string-face ((,class (:inherit ,font-lock-string-face))))
     (font-latex-math-face ((,class (:foreground ,zenburn-orange))))

     ;; auto-complete
     (ac-candidate-face ((,class (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
     (ac-selection-face ((,class (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
     (popup-tip-face ((,class (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
     (popup-scroll-bar-foreground-face ((,class (:background ,zenburn-blue-5))))
     (popup-scroll-bar-background-face ((,class (:background ,zenburn-bg-1))))
     (popup-isearch-match ((,class (:background ,zenburn-bg :foreground ,zenburn-fg))))

     ;; diff
     (diff-added ((,class (:foreground ,zenburn-green+4 :background nil))
                  (t (:foreground ,zenburn-green-1 :background nil))))
     (diff-changed ((,class (:foreground ,zenburn-yellow))))
     (diff-removed ((,class (:foreground ,zenburn-red :background nil))
                    (t (:foreground ,zenburn-red-3 :background nil))))
     (diff-refine-added ((,class (:inherit diff-added :weight bold))))
     (diff-refine-change ((,class (:inherit diff-changed :weight bold))))
     (diff-refine-removed ((,class (:inherit diff-removed :weight bold))))
     (diff-header ((,class (:background ,zenburn-bg+2))
                   (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
     (diff-file-header
      ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :bold t))
       (t (:background ,zenburn-fg :foreground ,zenburn-bg :bold t))))

     ;; ediff
     (ediff-current-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (ediff-current-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-4))))
     (ediff-current-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green-1))))
     (ediff-current-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-5))))
     (ediff-even-diff-A ((,class (:background ,zenburn-bg+1))))
     (ediff-even-diff-Ancestor ((,class (:background ,zenburn-bg+1))))
     (ediff-even-diff-B ((,class (:background ,zenburn-bg+1))))
     (ediff-even-diff-C ((,class (:background ,zenburn-bg+1))))
     (ediff-fine-diff-A ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 :weight bold))))
     (ediff-fine-diff-Ancestor ((,class (:foreground ,zenburn-fg :background ,zenburn-red-2 weight bold))))
     (ediff-fine-diff-B ((,class (:foreground ,zenburn-fg :background ,zenburn-green :weight bold))))
     (ediff-fine-diff-C ((,class (:foreground ,zenburn-fg :background ,zenburn-blue-3 :weight bold ))))
     (ediff-odd-diff-A ((,class (:background ,zenburn-bg+2))))
     (ediff-odd-diff-Ancestor ((,class (:background ,zenburn-bg+2))))
     (ediff-odd-diff-B ((,class (:background ,zenburn-bg+2))))
     (ediff-odd-diff-C ((,class (:background ,zenburn-bg+2))))

     ;; eshell
     (eshell-prompt ((,class (:foreground ,zenburn-yellow :weight bold))))
     (eshell-ls-archive ((,class (:foreground ,zenburn-red-1 :weight bold))))
     (eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     (eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     (eshell-ls-directory ((,class (:foreground ,zenburn-blue+1 :weight bold))))
     (eshell-ls-executable ((,class (:foreground ,zenburn-red+1 :weight bold))))
     (eshell-ls-unreadable ((,class (:foreground ,zenburn-fg))))
     (eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     (eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     (eshell-ls-special ((,class (:foreground ,zenburn-yellow :weight bold))))
     (eshell-ls-symlink ((,class (:foreground ,zenburn-cyan :weight bold))))

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
     (flycheck-fringe-error ((,class (:foreground ,zenburn-red-1 :weight bold))))
     (flycheck-fringe-warning ((,class (:foreground ,zenburn-yellow :weight bold))))
     (flycheck-fringe-info ((,class (:foreground ,zenburn-cyan :weight bold))))

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
     (rtags-errline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,zenburn-red :foreground ,zenburn-bg)
                                `(:underline (:color ,zenburn-red :style wave))))))
     (rtags-warnline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,zenburn-orange :foreground ,zenburn-bg)
                                 `(:underline (:color ,zenburn-orange :style wave))))))
     (rtags-fixitline ((,class ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,zenburn-green :foreground ,zenburn-bg)
                                  `(:underline (:color ,zenburn-green :style wave))))))

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
     (ack-separator ((,class (:foreground ,zenburn-fg))))
     (ack-file ((,class (:foreground ,zenburn-blue))))
     (ack-line ((,class (:foreground ,zenburn-yellow))))
     (ack-match ((,class (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))

     ;; git-gutter
     (git-gutter:added ((,class (:foreground ,zenburn-green :weight bold :inverse-video t))))
     (git-gutter:deleted ((,class (:foreground ,zenburn-red :weight bold :inverse-video t))))
     (git-gutter:modified ((,class (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
     (git-gutter:unchanged ((,class (:foreground ,zenburn-fg :weight bold :inverse-video t))))

     ;; git-gutter-fr
     (git-gutter-fr:added ((,class (:foreground ,zenburn-green  :weight bold))))
     (git-gutter-fr:deleted ((,class (:foreground ,zenburn-red :weight bold))))
     (git-gutter-fr:modified ((,class (:foreground ,zenburn-magenta :weight bold))))

     ;; git-rebase-mode
     (git-rebase-hash ((,class (:foreground ,zenburn-orange))))

     ;; helm
     (helm-header
      ((,class (:foreground ,zenburn-green :background ,zenburn-bg
                :underline nil :box nil))))
     (helm-source-header
      ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg-1
                :underline nil :weight bold
                :box (:line-width -1 :style released-button)))))
     (helm-selection ((,class (:background ,zenburn-bg+1 :underline nil))))
     (helm-selection-line ((,class (:background ,zenburn-bg+1))))
     (helm-visible-mark ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
     (helm-candidate-number ((,class (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
     (helm-separator ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-time-zone-current ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-time-zone-home ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-bookmark-addressbook ((,class (:foreground ,zenburn-orange :background ,zenburn-bg))))
     (helm-bookmark-directory ((,class (:foreground nil :background nil :inherit helm-ff-directory))))
     (helm-bookmark-file ((,class (:foreground nil :background nil :inherit helm-ff-file))))
     (helm-bookmark-gnus ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg))))
     (helm-bookmark-info ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-bookmark-man ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg))))
     (helm-bookmark-w3m ((,class (:foreground ,zenburn-magenta :background ,zenburn-bg))))
     (helm-buffer-not-saved ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-buffer-process ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-buffer-saved-out ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (helm-buffer-size ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-ff-directory ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
     (helm-ff-file ((,class (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
     (helm-ff-executable ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
     (helm-ff-invalid-symlink ((,class (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
     (helm-ff-symlink ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
     (helm-ff-prefix ((,class (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
     (helm-grep-cmd-line ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-grep-file ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (helm-grep-finish ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
     (helm-grep-lineno ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     (helm-grep-running ((,class (:foreground ,zenburn-red :background ,zenburn-bg))))
     (helm-moccur-buffer ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg))))
     (helm-mu-contacts-address-face ((,class (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
     (helm-mu-contacts-name-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
     (helm-swoop-target-word-face ((,class (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))))

     ;; hl-line-mode
     (hl-line-face ((,class (:background ,zenburn-bg-05))
                    (t :weight bold)))
     (hl-line ((,class (:background ,zenburn-bg-05)) ; old emacsen
               (t :weight bold)))

     ;; hl-sexp
     (hl-sexp-face ((,class (:background ,zenburn-bg+1))
                    (t :weight bold)))

     ;; IDO
     (ido-first-match ((,class (:foreground ,zenburn-yellow :weight bold))))
     (ido-only-match ((,class (:foreground ,zenburn-orange :weight bold))))
     (ido-subdir ((,class (:foreground ,zenburn-yellow))))
     (ido-indicator ((,class (:foreground ,zenburn-yellow :background ,zenburn-red-4))))

     ;; iedit-mode
     (iedit-occurrence ((,class (:background ,zenburn-bg+2 :weight bold))))

     ;; js2-mode
     (js2-warning ((,class (:underline ,zenburn-orange))))
     (js2-error ((,class (:foreground ,zenburn-red :weight bold))))
     (js2-jsdoc-tag ((,class (:foreground ,zenburn-green-1))))
     (js2-jsdoc-type ((,class (:foreground ,zenburn-green+2))))
     (js2-jsdoc-value ((,class (:foreground ,zenburn-green+3))))
     (js2-function-param ((,class (:foreground ,zenburn-green+3))))
     (js2-external-variable ((,class (:foreground ,zenburn-orange))))

     ;; linum-mode
     (linum ((,class (:foreground ,zenburn-green+2 :background ,zenburn-bg))))

     ;; magit
     (magit-item-highlight ((,class (:background ,zenburn-bg+05))))
     (magit-section-title ((,class (:foreground ,zenburn-yellow :weight bold))))
     (magit-process-ok ((,class (:foreground ,zenburn-green :weight bold))))
     (magit-process-ng ((,class (:foreground ,zenburn-red :weight bold))))
     (magit-branch ((,class (:foreground ,zenburn-blue :weight bold))))
     (magit-log-author ((,class (:foreground ,zenburn-orange))))
     (magit-log-sha1 ((,class (:foreground ,zenburn-orange))))

     ;; mic-paren
     (paren-face-match ((,class (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
     (paren-face-mismatch ((,class (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))
     (paren-face-no-match ((,class (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))))

     ;; org-mode
     (org-agenda-date-today
      ((,class (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
     (org-agenda-structure
      ((,class (:inherit font-lock-comment-face))))
     (org-archived ((,class (:foreground ,zenburn-fg :weight bold))))
     (org-checkbox ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                             :box (:line-width 1 :style released-button)))))
     (org-date ((,class (:foreground ,zenburn-blue :underline t))))
     (org-deadline-announce ((,class (:foreground ,zenburn-red-1))))
     (org-done ((,class (:bold t :weight bold :foreground ,zenburn-green+3))))
     (org-formula ((,class (:foreground ,zenburn-yellow-2))))
     (org-headline-done ((,class (:foreground ,zenburn-green+3))))
     (org-hide ((,class (:foreground ,zenburn-bg-1))))
     (org-document-title ((,class
                           ,(append `(:weight bold :foreground ,zenburn-green+2)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))
     (org-level-1 ((,class
                    ,(append `(:foreground ,zenburn-orange)
                             (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))
     (org-level-2 ((,class (:foreground ,zenburn-green+4))))
     (org-level-3 ((,class (:foreground ,zenburn-blue-1))))
     (org-level-4 ((,class (:foreground ,zenburn-yellow-2))))
     (org-level-5 ((,class (:foreground ,zenburn-cyan))))
     (org-level-6 ((,class (:foreground ,zenburn-green+2))))
     (org-level-7 ((,class (:foreground ,zenburn-red-4))))
     (org-level-8 ((,class (:foreground ,zenburn-blue-4))))
     (org-link ((,class (:foreground ,zenburn-yellow-2 :underline t))))
     (org-scheduled ((,class (:foreground ,zenburn-green+4))))
     (org-scheduled-previously ((,class (:foreground ,zenburn-red))))
     (org-scheduled-today ((,class (:foreground ,zenburn-blue+1))))
     (org-sexp-date ((,class (:foreground ,zenburn-blue+1 :underline t))))
     (org-special-keyword ((,class (:inherit font-lock-comment-face))))
     (org-table ((,class (:foreground ,zenburn-green+2))))
     (org-tag ((,class (:bold t :weight bold))))
     (org-time-grid ((,class (:foreground ,zenburn-orange))))
     (org-todo ((,class (:bold t :foreground ,zenburn-red :weight bold))))
     (org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
     (org-warning ((,class (:bold t :foreground ,zenburn-red :weight bold :underline nil))))
     (org-column ((,class (:background ,zenburn-bg-1))))
     (org-column-title ((,class (:background ,zenburn-bg-1 :underline t :weight bold))))
     (org-mode-line-clock ((,class (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
     (org-mode-line-clock-overrun ((,class (:foreground ,zenburn-bg :background ,zenburn-red-1))))
     (org-ellipsis ((,class (:foreground ,zenburn-yellow-1 :underline t))))
     (org-footnote ((,class (:foreground ,zenburn-cyan :underline t))))

     ;; outline
     (outline-1 ((,class (:foreground ,zenburn-orange))))
     (outline-2 ((,class (:foreground ,zenburn-green+4))))
     (outline-3 ((,class (:foreground ,zenburn-blue-1))))
     (outline-4 ((,class (:foreground ,zenburn-yellow-2))))
     (outline-5 ((,class (:foreground ,zenburn-cyan))))
     (outline-6 ((,class (:foreground ,zenburn-green+2))))
     (outline-7 ((,class (:foreground ,zenburn-red-4))))
     (outline-8 ((,class (:foreground ,zenburn-blue-4))))

     ;; Clojure
     (clojure-test-failure-face ((,class (:foreground ,zenburn-orange :weight bold :underline t))))
     (clojure-test-error-face ((,class (:foreground ,zenburn-red :weight bold :underline t))))
     (clojure-test-success-face ((,class (:foreground ,zenburn-green+1 :weight bold :underline t))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,zenburn-fg))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,zenburn-green+4))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,zenburn-yellow-2))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,zenburn-cyan))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,zenburn-green+2))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,zenburn-blue+1))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,zenburn-yellow-1))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,zenburn-green+1))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,zenburn-blue-2))))
     (rainbow-delimiters-depth-10-face ((,class (:foreground ,zenburn-orange))))
     (rainbow-delimiters-depth-11-face ((,class (:foreground ,zenburn-green))))
     (rainbow-delimiters-depth-12-face ((,class (:foreground ,zenburn-blue-5))))

     ;; sh-mode
     (sh-hezenburn-redoc     ((,class (:foreground ,zenburn-yellow :bold t))))
     (sh-quoted-exec ((,class (:foreground ,zenburn-red))))

     ;; show-paren
     (show-paren-mismatch ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
     (show-paren-match ((,class (:background ,zenburn-bg+3 :weight bold))))

     ;; smartparens
     (sp-show-pair-mismatch-face ((,class (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
     (sp-show-pair-match-face ((,class (:background ,zenburn-bg+3 :weight bold))))

     ;; sml-mode-line
     '(sml-modeline-end-face ((,class :inherit default :width condensed)))

     ;; SLIME
     (slime-repl-output-face ((,class (:foreground ,zenburn-red))))
     (slime-repl-inputed-output-face ((,class (:foreground ,zenburn-green))))
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
     (slime-highlight-face ((,class (:inherit highlight))))

     ;; speedbar
     (speedbar-button-face ((,class (:foreground ,zenburn-green+2))))
     (speedbar-directory-face ((,class (:foreground ,zenburn-cyan))))
     (speedbar-file-face ((,class (:foreground ,zenburn-fg))))
     (speedbar-highlight-face ((,class (:foreground ,zenburn-bg :background ,zenburn-green+2))))
     (speedbar-selected-face ((,class (:foreground ,zenburn-red))))
     (speedbar-separator-face ((,class (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
     (speedbar-tag-face ((,class (:foreground ,zenburn-yellow))))

     ;; tabbar
     (tabbar-button ((,class (:foreground ,zenburn-fg :background ,zenburn-bg))))
     (tabbar-selected ((,class (:foreground ,zenburn-fg :background ,zenburn-bg
                                :box (:line-width -1 :style pressed-button)))))
     (tabbar-unselected ((,class (:foreground ,zenburn-fg :background ,zenburn-bg+1
                                  :box (:line-width -1 :style released-button)))))

     ;; term
     (term-color-black ((,class (:foreground ,zenburn-bg :background ,zenburn-bg-1))))
     (term-color-red ((,class (:foreground ,zenburn-red-2 :background ,zenburn-red-4))))
     (term-color-green ((,class (:foreground ,zenburn-green :background ,zenburn-green+2))))
     (term-color-yellow ((,class (:foreground ,zenburn-orange :background ,zenburn-yellow))))
     (term-color-blue ((,class (:foreground ,zenburn-blue-1 :background ,zenburn-blue-4))))
     (term-color-magenta ((,class (:foreground ,zenburn-magenta :background ,zenburn-red))))
     (term-color-cyan ((,class (:foreground ,zenburn-cyan :background ,zenburn-blue))))
     (term-color-white ((,class (:foreground ,zenburn-fg :background ,zenburn-fg-1))))
     (term-default-fg-color ((,class (:inherit term-color-white))))
     (term-default-bg-color ((,class (:inherit term-color-black))))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,zenburn-fg+1 :weight bold))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,zenburn-red-1 :weight bold))))
     (undo-tree-visualizer-default-face ((,class (:foreground ,zenburn-fg))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,zenburn-yellow))))
     (undo-tree-visualizer-unmodified-face ((,class (:foreground ,zenburn-cyan))))

     ;; whitespace-mode
     (whitespace-space ((,class (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
     (whitespace-hspace ((,class (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
     (whitespace-tab ((,class (:background ,zenburn-red-1))))
     (whitespace-newline ((,class (:foreground ,zenburn-bg+1))))
     (whitespace-trailing ((,class (:background ,zenburn-red))))
     (whitespace-line ((,class (:background ,zenburn-bg :foreground ,zenburn-magenta))))
     (whitespace-space-before-tab ((,class (:background ,zenburn-orange :foreground ,zenburn-orange))))
     (whitespace-indentation ((,class (:background ,zenburn-yellow :foreground ,zenburn-red))))
     (whitespace-empty ((,class (:background ,zenburn-yellow))))
     (whitespace-space-after-tab ((,class (:background ,zenburn-yellow :foreground ,zenburn-red))))

     ;; which-func-mode
     (which-func ((,class (:foreground ,zenburn-green+4))))

     ;; yascroll
     (yascroll:thumb-text-area ((,class (:background ,zenburn-bg-1))))
     (yascroll:thumb-fringe ((,class (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
     )))

(defun define-zenburn-theme ()
  "Define the zenburn theme (only one variant for now)"
  (deftheme zenburn "A low contrast theme")
  (with-zenburn-colors
   (apply 'custom-theme-set-faces 'zenburn (zenburn-face-specs)))
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
