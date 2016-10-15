;;; color-theme-atom-one.el --- based on the colors from the Atom One theme
;;;
;;; Credit: inspired by:
;;; https://github.com/atom/one-dark-ui
;;; https://github.com/jonathanchu/atom-one-dark-theme

(require 'init-prefs)

;;; Theme options

(defcustom exordium-atom-one-search-box t
  "Enable displaying a box around search occurrences.
This is similar to how Atom displays them, but it makes the line
width change as search progress which can be distracting. Set
this to nil for emacs-like search results."
  :group 'exordium
  :type  'boolean)

;;; Color palette

(defconst atom-one-colors
  '((atom-one-dark-accent   . "#528BFF")
    (atom-one-dark-fg       . "#ABB2BF")
    (atom-one-dark-bg       . "#282C34")
    (atom-one-dark-bg-1     . "#21252b")
    (atom-one-dark-bg-hl    . "#2F343D")
    (atom-one-dark-mono-1   . "#ABB2BF")
    (atom-one-dark-mono-2   . "#828997")
    (atom-one-dark-mono-3   . "#5C6370")
    (atom-one-dark-cyan     . "#56B6C2")
    (atom-one-dark-blue     . "#61AFEF")
    (atom-one-dark-purple   . "#C678DD")
    (atom-one-dark-green    . "#98C379")
    (atom-one-dark-red-1    . "#E06C75")
    (atom-one-dark-red-2    . "#BE5046")
    (atom-one-dark-orange-1 . "#D19A66")
    (atom-one-dark-orange-2 . "#E5C07B")
    (atom-one-dark-gray     . "#3E4451")
    (atom-one-dark-silver   . "#AAAAAA")
    (atom-one-dark-black    . "#0F1011")))

;;; Theme definition

(defmacro with-atom-one-colors (&rest body)
  "Execute `BODY' in a scope with variables bound to the atom one colors."
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   atom-one-colors))
     ,@body))

(defmacro atom-one-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:slant italic :weight bold))))
     (underline ((t (:underline t))))
     (italic ((t (:slant italic))))
     (shadow ((t (:background ,atom-one-dark-bg-1))))
     (success ((t (:foreground ,atom-one-dark-green))))
     (error ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
     (warning ((t (:foreground ,atom-one-dark-orange-2))))
     (scroll-bar ((t (:background ,atom-one-dark-bg))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((t (:background ,atom-one-dark-blue :foreground ,atom-one-dark-bg-hl))))
     (show-paren-mismatch ((t (:background ,atom-one-dark-red-1 :foreground ,atom-one-dark-bg-hl))))

     ;; Region
     (region ((t (:background ,atom-one-dark-gray))))
     (secondary-selection ((t (:background ,atom-one-dark-bg-1))))

     ;; Font lock
     (font-lock-builtin-face ((t (:foreground ,atom-one-dark-cyan))))
     (font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
     (font-lock-comment-face ((t (:foreground ,atom-one-dark-mono-3 :slant italic))))
     (font-lock-constant-face ((t (:foreground ,atom-one-dark-orange-1))))
     (font-lock-doc-face ((t (:foreground ,atom-one-dark-mono-3 :slant italic))))
     (font-lock-function-name-face ((t (:foreground ,atom-one-dark-blue))))
     (font-lock-keyword-face ((t (:foreground ,atom-one-dark-purple))))
     (font-lock-negation-char-face ((t (:foreground ,atom-one-dark-blue))))
     (font-lock-preprocessor-face ((t (:foreground ,atom-one-dark-mono-2))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,atom-one-dark-orange-2))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,atom-one-dark-purple))))
     (font-lock-string-face ((t (:foreground ,atom-one-dark-green))))
     (font-lock-type-face ((t (:foreground ,atom-one-dark-orange-2))))
     (font-lock-variable-name-face ((t (:foreground ,atom-one-dark-fg))))
     (font-lock-warning-face ((t (:foreground ,atom-one-dark-red-1 :bold t))))

     ;; Emacs interface
     (cursor ((t (:background ,atom-one-dark-accent))))
     (fringe ((t (:background ,atom-one-dark-bg))))

     (linum ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3
                 :box nil :underline nil))))
     (linum-highlight-face ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-1
                                :box nil :underline nil))))

     (border ((t (:foreground ,atom-one-dark-mono-3))))
     (vertical-border ((t (:foreground ,atom-one-dark-mono-3))))
     (border-glyph ((t (nil))))

     (hl-line ((t (:background ,atom-one-dark-bg-hl))))
     (highlight ((t (:background ,atom-one-dark-gray))))

     (link ((t (:foreground ,atom-one-dark-blue :underline t :weight bold))))
     (link-visited ((t (:foreground ,atom-one-dark-blue :underline t :weight normal))))

     (menu ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg-1))))

     (query-replace ((t (:inherit (isearch)))))
     (minibuffer-prompt ((t (:foreground ,atom-one-dark-silver))))

     ;; Customize
     (custom-variable-tag ((t (:foreground ,atom-one-dark-blue))))
     (custom-group-tag ((t (:foreground ,atom-one-dark-blue))))
     (custom-state ((t (:foreground ,atom-one-dark-green))))

     ;; Mode-line
     (mode-line ((t (:background ,atom-one-dark-gray :foreground ,atom-one-dark-silver))))
     (mode-line-buffer-id ((t (:weight bold))))
     (mode-line-emphasis ((t (:weight bold))))
     (mode-line-inactive ((t (:background ,atom-one-dark-gray))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,atom-one-dark-gray))))
     (exordium-powerline-active2 ((t (:background ,atom-one-dark-bg-1))))
     (exordium-powerline-active3 ((t (:background ,atom-one-dark-purple :foreground ,atom-one-dark-bg))))
     (exordium-powerline-active4 ((t (:background ,atom-one-dark-red-1 :foreground ,atom-one-dark-bg))))
     (exordium-powerline-active5 ((t (:background ,atom-one-dark-green :foreground ,atom-one-dark-bg))))
     (exordium-powerline-inactive1 ((t (:background ,atom-one-dark-gray :foreground ,atom-one-dark-mono-3))))
     (exordium-powerline-inactive2 ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-3))))
     (exordium-powerline-inactive3 ((t (:background ,atom-one-dark-mono-3 :foreground ,atom-one-dark-bg))))
     (exordium-project-name ((t (:foreground ,atom-one-dark-purple))))

     ;; Search
     (match ((t ,(if exordium-atom-one-search-box
                     `(:foreground ,atom-one-dark-fg :background ,atom-one-dark-gray
                       :box (:color ,atom-one-dark-fg :line-width 1))
                   `(:foreground ,atom-one-dark-bg :background ,atom-one-dark-blue :box nil)))))
     (isearch ((t ,(if exordium-atom-one-search-box
                       `(:foreground ,atom-one-dark-fg :background ,atom-one-dark-gray
                                     :box (:color ,atom-one-dark-blue :line-width 1))
                     `(:foreground ,atom-one-dark-bg :background ,atom-one-dark-orange-1 :box nil)))))
     (isearch-fail ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-red-1))))
     (lazy-highlight ((t ,(if exordium-atom-one-search-box
                              `(:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg-1
                                :box (:color ,atom-one-dark-fg :line-width 1))
                     `(:foreground ,atom-one-dark-bg :background ,atom-one-dark-blue :box nil)))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,atom-one-dark-orange-1 :background ,atom-one-dark-bg :inverse-video t))))
     (hi-pink ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-bg :inverse-video t))))
     (hi-green ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg :inverse-video t))))
     (hi-blue ((t (:foreground ,atom-one-dark-blue :background ,atom-one-dark-bg :inverse-video t))))

     ;; IDO
     (ido-subdir ((t (:foreground ,atom-one-dark-mono-3))))
     (ido-first-match ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
     (ido-only-match ((t (:foreground ,atom-one-dark-green :weight bold))))
     (ido-indicator ((t (:foreground ,atom-one-dark-red-1))))
     (ido-virtual ((t (:foreground ,atom-one-dark-mono-3))))

     ;; Helm
     (helm-header ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-bg :underline nil
                       :box (:line-width 1 :color ,atom-one-dark-gray)))))
     (helm-source-header ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg
                              :underline nil :weight bold
                              :box (:line-width 1 :color ,atom-one-dark-gray)))))
     (helm-selection ((t (:background ,atom-one-dark-gray))))
     (helm-selection-line ((t (:background ,atom-one-dark-gray))))
     (helm-visible-mark ((t (:foreground ,atom-one-dark-bg :foreground ,atom-one-dark-orange-2))))
     (helm-candidate-number ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg-1))))
     (helm-separator ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-red-1))))
     (helm-M-x-key ((t (:foreground ,atom-one-dark-orange-1))))
     (helm-bookmark-addressbook ((t (:foreground ,atom-one-dark-orange-1))))
     (helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
     (helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
     (helm-bookmark-gnus ((t (:foreground ,atom-one-dark-purple))))
     (helm-bookmark-info ((t (:foreground ,atom-one-dark-green))))
     (helm-bookmark-man ((t (:foreground ,atom-one-dark-orange-2))))
     (helm-bookmark-w3m ((t (:foreground ,atom-one-dark-purple))))
     (helm-match ((t (:foreground ,atom-one-dark-purple))))
     (helm-ff-directory ((t (:foreground ,atom-one-dark-cyan :background ,atom-one-dark-bg :weight bold))))
     (helm-ff-file ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg :weight normal))))
     (helm-ff-executable ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg :weight normal))))
     (helm-ff-invalid-symlink ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg :weight bold))))
     (helm-ff-symlink ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg :weight bold))))
     (helm-ff-prefix ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-orange-2 :weight normal))))
     (helm-buffer-not-saved ((t (:foreground ,atom-one-dark-red-1))))
     (helm-buffer-process ((t (:foreground ,atom-one-dark-mono-2))))
     (helm-buffer-saved-out ((t (:foreground ,atom-one-dark-fg))))
     (helm-buffer-size ((t (:foreground ,atom-one-dark-mono-2))))
     (helm-buffer-directory ((t (:foreground ,atom-one-dark-purple))))
     (helm-grep-cmd-line ((t (:foreground ,atom-one-dark-cyan))))
     (helm-grep-file ((t (:foreground ,atom-one-dark-fg))))
     (helm-grep-finish ((t (:foreground ,atom-one-dark-green))))
     (helm-grep-lineno ((t (:foreground ,atom-one-dark-mono-2))))
     (helm-grep-finish ((t (:foreground ,atom-one-dark-red-1))))
     (helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))

     (helm-swoop-target-line-face ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-orange-1))))
     (helm-swoop-target-word-face ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-blue))))

     ;; Flymake
     (flymake-warnline ((t (:underline ,atom-one-dark-orange-2 :background ,atom-one-dark-bg))))
     (flymake-errline ((t (:underline ,atom-one-dark-red-2 :background ,atom-one-dark-bg))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-line-number ((t (:foreground ,atom-one-dark-mono-2))))
     (compilation-column-number ((t (:foreground ,atom-one-dark-mono-2))))
     (compilation-mode-line-exit ((t (:foreground ,atom-one-dark-green))))
     (compilation-mode-line-fail ((t (:foreground ,atom-one-dark-red-1))))
     (compilation-mode-line-run ((t (:foreground ,atom-one-dark-blue))))

     ;; RTags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                             `(:background ,atom-one-dark-red-2 :foreground ,atom-one-dark-bg)
                           `(:underline (:color ,atom-one-dark-red-2 :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                              `(:background ,atom-one-dark-orange-2 :foreground ,atom-one-dark-bg)
                            `(:underline (:color ,atom-one-dark-orange-2 :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                               `(:background ,atom-one-dark-green :foreground ,atom-one-dark-bg)
                             `(:underline (:color ,atom-one-dark-green :style wave))))))
     (rtags-skippedline ((t (:background ,atom-one-dark-bg-1))))

     ;; Magit
     (magit-section-highlight ((t (:background ,atom-one-dark-bg-hl))))
     (magit-section-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))
     (magit-section-heading-selection ((t (:foreground ,atom-one-dark-fg :weight bold))))
     (magit-diff-file-heading ((t (:weight bold))))
     (magit-diff-file-heading-highlight ((t (:background ,atom-one-dark-gray :weight bold))))
     (magit-diff-file-heading-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-hl :weight bold))))
     (magit-diff-hunk-heading ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-gray))))
     (magit-diff-hunk-heading-highlight ((t (:foreground ,atom-one-dark-mono-1 :background ,atom-one-dark-mono-3))))
     (magit-diff-hunk-heading-selection ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-mono-3))))
     (magit-diff-context ((t (:foreground ,atom-one-dark-fg))))
     (magit-diff-context-highlight ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-fg))))
     (magit-diffstat-added ((t (:foreground ,atom-one-dark-green))))
     (magit-diffstat-removed ((t (:foreground ,atom-one-dark-red-1))))
     (magit-process-ok ((t (:foreground ,atom-one-dark-green))))
     (magit-process-ng ((t (:foreground ,atom-one-dark-red-1))))
     (magit-log-author ((t (:foreground ,atom-one-dark-orange-2))))
     (magit-log-date ((t (:foreground ,atom-one-dark-mono-2))))
     (magit-log-graph ((t (:foreground ,atom-one-dark-silver))))
     (magit-sequence-pick ((t (:foreground ,atom-one-dark-orange-2))))
     (magit-sequence-stop ((t (:foreground ,atom-one-dark-green))))
     (magit-sequence-part ((t (:foreground ,atom-one-dark-orange-1))))
     (magit-sequence-head ((t (:foreground ,atom-one-dark-blue))))
     (magit-sequence-drop ((t (:foreground ,atom-one-dark-red-1))))
     (magit-sequence-done ((t (:foreground ,atom-one-dark-mono-2))))
     (magit-sequence-onto ((t (:foreground ,atom-one-dark-mono-2))))
     (magit-bisect-good ((t (:foreground ,atom-one-dark-green))))
     (magit-bisect-skip ((t (:foreground ,atom-one-dark-orange-1))))
     (magit-bisect-bad ((t (:foreground ,atom-one-dark-red-1))))
     (magit-blame-heading ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
     (magit-blame-hash ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-purple))))
     (magit-blame-name ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-orange-2))))
     (magit-blame-date ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-3))))
     (magit-blame-summary ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
     (magit-dimmed ((t (:foreground ,atom-one-dark-mono-2))))
     (magit-hash ((t (:foreground ,atom-one-dark-purple))))
     (magit-tag ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
     (magit-branch-remote ((t (:foreground ,atom-one-dark-green :weight bold))))
     (magit-branch-local ((t (:foreground ,atom-one-dark-blue :weight bold))))
     (magit-branch-current ((t (:foreground ,atom-one-dark-blue :weight bold :box t))))
     (magit-head ((t (:foreground ,atom-one-dark-blue :weight bold))))
     (magit-refname ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
     (magit-refname-stash ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
     (magit-refname-wip ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
     (magit-signature-good ((t (:foreground ,atom-one-dark-green))))
     (magit-signature-bad ((t (:foreground ,atom-one-dark-red-1))))
     (magit-signature-untrusted ((t (:foreground ,atom-one-dark-orange-1))))
     (magit-cherry-unmatched ((t (:foreground ,atom-one-dark-cyan))))
     (magit-cherry-equivalent ((t (:foreground ,atom-one-dark-purple))))
     (magit-reflog-commit ((t (:foreground ,atom-one-dark-green))))
     (magit-reflog-amend ((t (:foreground ,atom-one-dark-purple))))
     (magit-reflog-merge ((t (:foreground ,atom-one-dark-green))))
     (magit-reflog-checkout ((t (:foreground ,atom-one-dark-blue))))
     (magit-reflog-reset ((t (:foreground ,atom-one-dark-red-1))))
     (magit-reflog-rebase ((t (:foreground ,atom-one-dark-purple))))
     (magit-reflog-cherry-pick ((t (:foreground ,atom-one-dark-green))))
     (magit-reflog-remote ((t (:foreground ,atom-one-dark-cyan))))
     (magit-reflog-other ((t (:foreground ,atom-one-dark-cyan))))

     ;; Git gutter fringe
     (git-gutter-fr:added ((t (:foreground ,atom-one-dark-green :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
     (git-gutter-fr:modified ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))

     ;; Git commit
     (git-commit-comment-action ((t (:foreground ,atom-one-dark-green :weight bold))))
     (git-commit-comment-branch ((t (:foreground ,atom-one-dark-blue :weight bold))))
     (git-commit-comment-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))

     ;; Diff
     (diff-added ((t (:foreground ,atom-one-dark-green))))
     (diff-changed ((t (:foreground ,atom-one-dark-orange-1))))
     (diff-removed ((t (:foreground ,atom-one-dark-red-1))))
     (diff-header ((t (:background ,atom-one-dark-bg-hl))))
     (diff-file-header ((t (:background ,atom-one-dark-gray))))
     (diff-hunk-header ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-purple))))

     ;; Ediff
     (ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
     (ediff-odd-diff-A ((t (:foreground , :background nil :inverse-video t))))
     (ediff-odd-diff-B ((t (:foreground ,atom-one-dark-mono-3 :background nil :inverse-video t))))

     ;; Grep
     (grep-context-face ((t (:foreground ,atom-one-dark-mono-3))))
     (grep-error-face ((t (:foreground ,atom-one-dark-red-1 :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,atom-one-dark-blue))))
     (grep-match-face ((t (:foreground nil :background nil :inherit match))))

     ;; Org
     (org-level-1 ((t ,(append `(:foreground ,atom-one-dark-orange-1)
                               (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-level-2 ((t (:foreground ,atom-one-dark-mono-1))))
     (org-level-3 ((t (:foreground ,atom-one-dark-mono-2))))
     (org-level-4 ((t (:foreground ,atom-one-dark-mono-3))))
     (org-agenda-structure ((t (:foreground ,atom-one-dark-purple))))
     (org-agenda-date ((t (:foreground ,atom-one-dark-blue :underline nil))))
     (org-agenda-done ((t (:foreground ,atom-one-dark-green))))
     (org-agenda-dimmed-todo-face ((t (:foreground ,atom-one-dark-mono-3))))
     (org-block ((t (:foreground ,atom-one-dark-orange-1))))
     (org-code ((t (:foreground ,atom-one-dark-orange-2))))
     (org-column ((t (:background ,atom-one-dark-bg-hl))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground ,atom-one-dark-purple :underline t))))
     (org-document-info ((t (:foreground ,atom-one-dark-cyan))))
     (org-document-info-keyword ((t (:foreground ,atom-one-dark-green))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,atom-one-dark-orange-1)
                                    (if exordium-theme-use-big-font '(:height 1.44) nil)))))
     (org-todo ((t (:foreground ,atom-one-dark-red-1 :weight bold :box nil))))
     (org-done ((t (:foreground ,atom-one-dark-green :weight bold :box nil))))
     (org-checkbox ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
     (org-ellipsis ((t (:foreground ,atom-one-dark-mono-3))))
     (org-footnote ((t (:foreground ,atom-one-dark-cyan))))
     (org-formula ((t (:foreground ,atom-one-dark-red-1))))
     (org-hide ((t (:foreground ,atom-one-dark-bg-hl))))
     (org-link ((t (:foreground ,atom-one-dark-blue))))
     (org-scheduled ((t (:foreground ,atom-one-dark-green))))
     (org-scheduled-previously ((t (:foreground ,atom-one-dark-orange-1))))
     (org-scheduled-today ((t (:foreground ,atom-one-dark-green))))
     (org-special-keyword ((t (:foreground ,atom-one-dark-orange-1))))
     (org-table ((t (:foreground ,atom-one-dark-fg))))
     (org-upcoming-deadline ((t (:foreground ,atom-one-dark-orange-1))))
     (org-warning ((t (:weight bold :foreground ,atom-one-dark-red-1))))

     ;; Markdown
     (markdown-url-face ((t (:inherit link :foreground ,atom-one-dark-purple :weight normal))))
     (markdown-link-face ((t (:foreground ,atom-one-dark-blue :underline nil :weight normal))))
     (markdown-header-face-1 ((t
                               ,(append `(:weight bold :foreground ,atom-one-dark-red-1)
                                        (if exordium-theme-use-big-font '(:height 1.44)) nil))))
     (markdown-header-face-2 ((t
                               ,(append `(:weight bold :foreground ,atom-one-dark-red-1)
                                        (if exordium-theme-use-big-font '(:height 1.2)) nil))))
     (markdown-header-face-3 ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
     (markdown-header-face-4 ((t (:foreground ,atom-one-dark-red-1 :weight normal))))
     (markdown-header-face-5 ((t (:foreground ,atom-one-dark-red-1 :weight bold :slant italic))))
     (markdown-header-delimiter-face ((t (:foreground ,atom-one-dark-red-1))))
     (markdown-bold-face ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
     (markdown-italic-face ((t (:foreground ,atom-one-dark-purple :weight normal :slant italic))))
     (markdown-list-face ((t (:foreground ,atom-one-dark-red-1 :weight normal))))
     (markdown-inline-code-face ((t (:foreground ,atom-one-dark-green :weight normal))))
     (markdown-markup-face ((t (:foreground ,atom-one-dark-mono-3))))
     (markdown-pre-face ((t (:foreground ,atom-one-dark-green))))

     ;; js2-mode
     (js2-warning ((t (:underline (:color ,atom-one-dark-orange-1 :style wave)))))
     (js2-error ((t (:foreground nil :underline (:color ,atom-one-dark-red-1 :style wave)))))
     (js2-external-variable ((t (:foreground ,atom-one-dark-orange-2))))
     (js2-function-param ((t (:foreground ,atom-one-dark-blue))))
     (js2-instance-member ((t (:foreground ,atom-one-dark-blue))))
     (js2-private-function-call ((t (:foreground ,atom-one-dark-red-1))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,atom-one-dark-orange-1 :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,atom-one-dark-red-1 :style wave)))))

     ;; nxml
     (nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-element-local-name ((t (:foreground ,atom-one-dark-red-1 :inherit font-lock-variable-name-face))))
     (nxml-namespace-attribute-xmlns (( t (:foreground ,atom-one-dark-orange-1))))
     (nxml-attribute-value (( t (:foreground ,atom-one-dark-purple))))
     (nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))

     ;; Undo-tree
     (undo-tree-visualizer-default-face ((t (:foreground ,atom-one-dark-fg))))
     (undo-tree-visualizer-current-face ((t (:foreground ,atom-one-dark-green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((t (:foreground ,atom-one-dark-red-1))))
     (undo-tree-visualizer-register-face ((t (:foreground ,atom-one-dark-orange-2))))

     ;; Trailing whitespaces
     (trailing-whitespace ((t (:background ,atom-one-dark-red-1 :foreground ,atom-one-dark-orange-1))))
     (whitespace-empty ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-orange-1))))
     (whitespace-hspace ((t (:background ,atom-one-dark-gray :foreground ,atom-one-dark-mono-3))))
     (whitespace-indentation ((t (:background ,atom-one-dark-orange-1 :foreground ,atom-one-dark-red-1))))
     (whitespace-line ((t (:background ,atom-one-dark-bg-hl :foreground ,atom-one-dark-purple))))
     (whitespace-newline ((t (:foreground ,atom-one-dark-mono-3))))
     (whitespace-space ((t (:background ,atom-one-dark-bg-hl :foreground ,atom-one-dark-mono-3))))
     (whitespace-space-after-tab ((t (:background ,atom-one-dark-orange-1 :foreground ,atom-one-dark-red-1))))
     (whitespace-space-before-tab ((t (:background ,atom-one-dark-orange-1 :foreground ,atom-one-dark-red-1))))
     (whitespace-tab ((t (:background ,atom-one-dark-gray :foreground ,atom-one-dark-mono-3))))
     (whitespace-trailing ((t (:background ,atom-one-dark-red-1 :foreground ,atom-one-dark-orange-1))))
     (hl-sexp-face ((t (:background ,atom-one-dark-bg-hl))))
     (highlight-80+ ((t (:background ,atom-one-dark-bg-hl))))

     ;; TODO: Lisp
     (eval-sexp-fu-flash ((t (:background ,atom-one-dark-orange-1 :foreground ,atom-one-dark-bg))))
     (eval-sexp-fu-flash-error ((t (:background ,atom-one-dark-red-1 :foreground ,atom-one-dark-bg))))

     ;; Auto-complete
     (ac-completion-face ((t (:background ,atom-one-dark-bg-1))))
     (ac-candidate-face ((t (:background ,atom-one-dark-bg-1 :ba))))
     (ac-selection-face ((t (:background ,atom-one-dark-blue :foreground "#000000"))))
     (popup-tip-face ((t (:background ,atom-one-dark-orange-2 :foreground "#000000"))))

     ;; Company-mode
     (company-tooltip ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg-1))))
     (company-tooltip-annotation ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-bg-1))))
     (company-tooltip-selection ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-gray))))
     (company-tooltip-mouse ((t (:background ,atom-one-dark-gray))))
     (company-tooltip-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-1))))
     (company-tooltip-common-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-gray))))
     (company-preview ((t (:background ,atom-one-dark-bg))))
     (company-preview-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg))))
     (company-scrollbar-fg ((t (:background ,atom-one-dark-mono-1))))
     (company-scrollbar-bg ((t (:background ,atom-one-dark-bg-1))))

     ;; ace-jump
     (ace-jump-face-background ((t (:foreground ,atom-one-dark-mono-3 :background ,atom-one-dark-bg-1 :inverse-video nil))))
     (ace-jump-face-foreground ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg-1 :inverse-video nil))))

     ;; Others
     (which-func ((t (:background nil :weight bold))))
     )))

(defun define-atom-one-theme ()
  "Define the atom one theme"
  (deftheme atom-one "A port of the default theme of the Atom editor")
  (with-atom-one-colors
   (apply 'custom-theme-set-faces 'atom-one (atom-one-face-specs)))
  (provide-theme 'atom-one))

(provide 'color-theme-atom-one)

;; Local Variables:
;; no-byte-compile: t
;; End:
