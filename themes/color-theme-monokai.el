;;; color-theme-monokai.el --- a fruity theme.
;;;
;;; Credit:
;;; Inspired by the "Monokai" theme from Kelvin Smith.

(require 'org)

;;; Color palette. Note: only variant of the theme for now ("default").

(defconst monokai-colors
  '((default .
      (;; Accented colors
       (yellow           . "#E6DB74")
       (orange           . "#FD971F")
       (red              . "#F92672")
       (magenta          . "#FD5FF0")
       (violet           . "#AE81FF")
       (blue             . "#66D9EF")
       (cyan             . "#A1EFE4")
       (green            . "#A6E22E")
       (gray             . "#474747")

       ;; Darker and lighter accented colors
       (yellow-hc        . "#F3EA98")
       (yellow-lc        . "#968B26")
       (orange-hc        . "#FEB257")
       (orange-lc        . "#A45E0A")
       (red-hc           . "#FC5C94")
       (red-lc           . "#A20C41")
       (magenta-hc       . "#FE87F4")
       (magenta-lc       . "#A41F99")
       (violet-hc        . "#C2A1FF")
       (violet-lc        . "#562AA6")
       (blue-hc          . "#8DE6F7")
       (blue-lc          . "#21889B")
       (cyan-hc          . "#BBF7EF")
       (cyan-lc          . "#349B8D")
       (green-hc         . "#C1F161")
       (green-lc         . "#67930F")
       (gray-lc          . "#333333")
       (gray-hc          . "#6b6b6b")

       ;; Adaptive colors
       (monokai-fg       . "#F8F8F2")
       (monokai-bg       . "#272822")
       (monokai-hl-line  . "#3E3D31")
       (monokai-hl       . "#49483E")
       (monokai-emph     . "#F8F8F0")
       (monokai-comments . "#75715E")

       ;; Adaptive higher/lower contrast accented colors
       (monokai-fg-hc    . "#141414")
       (monokai-fg-lc    . "#171A0B")))))

;;; Options

(defcustom monokai-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'monokai)

(defcustom monokai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monokai)

(defmacro with-monokai-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various monokai colors.
`MODE' should be set to either 'default, ..."
  `(let ((colors (or (cdr (assoc ,mode monokai-colors))
                     (error "no such theme flavor"))))
     (let ((yellow           (cdr (assoc 'yellow colors)))
           (orange           (cdr (assoc 'orange colors)))
           (red              (cdr (assoc 'red colors)))
           (magenta          (cdr (assoc 'magenta colors)))
           (violet           (cdr (assoc 'violet colors)))
           (blue             (cdr (assoc 'blue colors)))
           (cyan             (cdr (assoc 'cyan colors)))
           (green            (cdr (assoc 'green colors)))
           (gray             (cdr (assoc 'gray colors)))
           (yellow-hc        (cdr (assoc 'yellow-hc colors)))
           (yellow-lc        (cdr (assoc 'yellow-lc colors)))
           (orange-hc        (cdr (assoc 'orange-hc colors)))
           (orange-lc        (cdr (assoc 'orange-lc colors)))
           (red-hc           (cdr (assoc 'red-hc colors)))
           (red-lc           (cdr (assoc 'red-lc colors)))
           (magenta-hc       (cdr (assoc 'magenta-hc colors)))
           (magenta-lc       (cdr (assoc 'magenta-lc colors)))
           (violet-hc        (cdr (assoc 'violet-hc colors)))
           (violet-lc        (cdr (assoc 'violet-lc colors)))
           (blue-hc          (cdr (assoc 'blue-hc colors)))
           (blue-lc          (cdr (assoc 'blue-lc colors)))
           (cyan-hc          (cdr (assoc 'cyan-hc colors)))
           (cyan-lc          (cdr (assoc 'cyan-lc colors)))
           (green-hc         (cdr (assoc 'green-hc colors)))
           (green-lc         (cdr (assoc 'green-lc colors)))
           (gray-hc          (cdr (assoc 'gray-hc colors)))
           (gray-lc          (cdr (assoc 'gray-lc colors)))
           (monokai-fg       (cdr (assoc 'monokai-fg colors)))
           (monokai-bg       (cdr (assoc 'monokai-bg colors)))
           (monokai-hl-line  (cdr (assoc 'monokai-hl-line colors)))
           (monokai-hl       (cdr (assoc 'monokai-hl colors)))
           (monokai-emph     (cdr (assoc 'monokai-emph colors)))
           (monokai-comments (cdr (assoc 'monokai-comments colors)))
           (monokai-fg-hc    (cdr (assoc 'monokai-fg-hc colors)))
           (monokai-fg-lc    (cdr (assoc 'monokai-fg-lc colors)))

           (s-variable-pitch (if monokai-use-variable-pitch
                               'variable-pitch 'default))
           (class            '((class color) (min-colors 89))))
       ,@body)))

(defmacro monokai-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((,class (:foreground ,monokai-fg :background ,monokai-bg))))
     (shadow ((,class (:foreground ,monokai-comments))))
     (match ((,class (:background ,monokai-hl :foreground ,monokai-emph :weight bold))))
     (cursor ((,class (:foreground ,monokai-bg :background ,monokai-fg :inverse-video t))))
     (mouse ((,class (:foreground ,monokai-bg :background ,monokai-fg :inverse-video t))))
     (escape-glyph-face ((,class (:foreground ,red))))
     (fringe ((,class (:foreground ,monokai-fg :background ,monokai-bg))))
     (highlight ((,class (:background ,monokai-hl))))
     (link ((,class (:foreground ,blue :underline t :weight bold))))
     (link-visited ((,class (:foreground ,blue :underline t :weight normal))))
     (success ((,class (:foreground ,green ))))
     (warning ((,class (:foreground ,yellow ))))
     (error ((,class (:foreground ,orange))))
     (lazy-highlight ((,class (:foreground ,monokai-bg :background ,yellow :weight normal))))
     (escape-glyph ((,class (:foreground ,violet))))
     (button ((t (:underline t))))

     ;; font lock
     (font-lock-builtin-face ((,class (:foreground ,red :weight normal))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,monokai-comments :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,monokai-comments :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,violet))))
     (font-lock-doc-face ((,class (:foreground ,monokai-comments :slant italic))))
     (font-lock-function-name-face ((,class (:foreground ,green))))
     (font-lock-keyword-face ((,class (:foreground ,red :weight normal))))
     (font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
     (font-lock-preprocessor-face ((,class (:foreground ,red))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight normal))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,violet :weight normal))))
     (font-lock-string-face ((,class (:foreground ,yellow))))
     (font-lock-type-face ((,class (:foreground ,blue :italic nil))))
     (font-lock-variable-name-face ((,class (:foreground ,monokai-fg))))
     (font-lock-warning-face ((,class (:foreground ,orange :weight bold :slant italic :underline t))))
     (c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;; Powerline
     (exordium-powerline-active1 ((,class (:background ,monokai-hl))))
     (exordium-powerline-active2 ((,class (:background ,monokai-hl-line))))
     (exordium-powerline-active3 ((,class (:background ,violet :foreground ,monokai-bg))))
     (exordium-powerline-active4 ((,class (:background ,red :foreground ,monokai-bg))))
     (exordium-powerline-active5 ((,class (:background ,green :foreground ,monokai-bg))))
     (exordium-powerline-inactive1 ((,class (:background ,monokai-hl-line))))
     (exordium-powerline-inactive2 ((,class (:background ,monokai-bg))))
     (exordium-powerline-inactive3 ((,class (:background ,monokai-comments :foreground ,monokai-bg))))
     (exordium-project-name ((,class (:foreground ,violet))))

     ;; compilation
     (compilation-column-face ((,class (:foreground ,cyan :underline nil))))
     (compilation-column-number ((,class (:inherit font-lock-doc-face
                                          :foreground ,cyan :underline nil))))
     (compilation-enter-directory-face ((,class (:foreground ,green :underline nil))))
     (compilation-error ((,class (:inherit error :underline nil))))
     (compilation-error-face ((,class (:foreground ,red :underline nil))))
     (compilation-face ((,class (:foreground ,monokai-fg :underline nil))))
     (compilation-info ((,class (:foreground ,monokai-comments :underline nil :bold nil))))
     (compilation-info-face ((,class (:foreground ,blue :underline nil))))
     (compilation-leave-directory-face ((,class (:foreground ,green :underline nil))))
     (compilation-line-face ((,class (:foreground ,green :underline nil))))
     (compilation-line-number ((,class (:foreground ,green :underline nil))))
     (compilation-warning ((,class (:inherit warning :underline nil))))
     (compilation-warning-face ((,class (:foreground ,yellow :weight normal :underline nil))))
     (compilation-mode-line-exit ((,class (:inherit compilation-info
                                           :foreground ,green :weight bold))))
     (compilation-mode-line-fail ((,class (:inherit compilation-error
                                           :foreground ,red :weight bold))))
     (compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))

     ;; RTags
     (rtags-errline ((,class (:underline (:color ,red :style wave)))))
     (rtags-warnline ((,class (:underline (:color ,orange :style wave)))))
     (rtags-fixitline ((,class (:underline (:color ,green :style wave)))))

     ;; cua
     (cua-global-mark ((,class (:background ,yellow :foreground ,monokai-bg))))
     (cua-rectangle ((,class (:inherit region :background ,magenta :foreground ,monokai-bg))))
     (cua-rectangle-noselect ((,class (:inherit region
                                       :background ,monokai-hl :foreground ,monokai-comments))))

     ;; dired
     (dired-directory ((,class (:foreground ,blue :weight normal))))
     (dired-flagged ((,class (:foreground ,red))))
     (dired-header ((,class (:foreground ,monokai-bg :background ,blue))))
     (dired-ignored ((,class (:inherit shadow))))
     (dired-mark ((,class (:foreground ,yellow :weight bold))))
     (dired-marked ((,class (:foreground ,magenta :weight bold))))
     (dired-perm-write ((,class (:foreground ,monokai-fg :underline t))))
     (dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     (dired-warning ((,class (:foreground ,orange :underline t))))

     ;; grep
     (grep-context-face ((,class (:foreground ,monokai-fg))))
     (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground ,orange :weight bold))))

     ;; faces used by isearch
     (isearch ((,class (:foreground ,monokai-bg :background ,magenta :weight normal))))
     (isearch-fail ((,class (:foreground ,red :background ,monokai-bg :bold t))))

     ;; misc faces
     (menu ((,class (:foreground ,monokai-fg :background ,monokai-bg))))
     (minibuffer-prompt ((,class (:foreground ,blue))))

     (mode-line ((,class (:inverse-video unspecified
                          :underline unspecified
                          :foreground ,monokai-fg
                          :background ,monokai-hl-line
                          :box (:line-width 1 :color ,monokai-hl-line :style unspecified)))))
     (mode-line-buffer-id ((,class (:foreground ,green :weight bold))))
     (mode-line-inactive ((,class (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,monokai-comments
                                   :background ,monokai-bg
                                   :box (:line-width 1 :color ,monokai-hl-line :style unspecified)))))
     (header-line ((,class (:inverse-video unspecified
                            :underline unspecified
                            :foreground ,monokai-emph
                            :background ,monokai-hl
                            :box (:line-width 1 :color ,monokai-hl :style unspecified)))))
     (region ((,class (:background ,monokai-hl :inherit t))))
     (secondary-selection ((,class (:background ,monokai-hl :inherit t))))
     (trailing-whitespace ((,class (:background ,red))))
     (vertical-border ((,class (:foreground ,monokai-hl))))

     ;; auto-complete
     (ac-candidate-face ((,class (:background ,monokai-hl :foreground ,cyan))))
     (ac-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     (ac-candidate-mouse-face ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     (ac-completion-face ((,class (:foreground ,monokai-emph :underline t))))
     (ac-gtags-candidate-face ((,class (:background ,monokai-hl :foreground ,blue))))
     (ac-gtags-selection-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     (ac-yasnippet-candidate-face ((,class (:background ,monokai-hl :foreground ,yellow))))
     (ac-yasnippet-selection-face ((,class (:background ,yellow-lc :foreground ,yellow-hc))))

     ;; clojure-test-mode
     (clojure-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     (clojure-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     (clojure-test-success-face ((t (:foreground ,green :weight bold :underline t))))

     ;; diff
     (diff-added ((,class (:foreground ,green :background ,monokai-bg))))
     (diff-changed ((,class (:foreground ,blue :background ,monokai-bg))))
     (diff-removed ((,class (:foreground ,red :background ,monokai-bg))))
     (diff-header ((,class (:background ,monokai-bg))))
     (diff-file-header ((,class (:background ,monokai-bg :foreground ,monokai-fg :weight bold))))
     (diff-refine-added ((,class :foreground ,monokai-bg :background ,green)))
     (diff-refine-change ((,class :foreground ,monokai-bg :background ,blue)))
     (diff-refine-removed ((,class (:foreground ,monokai-bg :background ,red))))

     ;; ediff
     (ediff-fine-diff-A ((,class (:background ,orange-lc))))
     (ediff-fine-diff-B ((,class (:background ,green-lc))))
     (ediff-fine-diff-C ((,class (:background ,yellow-lc))))
     (ediff-current-diff-C ((,class (:background ,blue-lc))))
     (ediff-even-diff-A ((,class (:background ,monokai-comments :foreground ,monokai-fg-lc ))))
     (ediff-odd-diff-A ((,class (:background ,monokai-comments :foreground ,monokai-fg-hc ))))
     (ediff-even-diff-B ((,class (:background ,monokai-comments :foreground ,monokai-fg-hc ))))
     (ediff-odd-diff-B ((,class (:background ,monokai-comments :foreground ,monokai-fg-lc ))))
     (ediff-even-diff-C ((,class (:background ,monokai-comments :foreground ,monokai-fg ))))
     (ediff-odd-diff-C ((,class (:background ,monokai-comments :foreground ,monokai-bg ))))

     ;; flymake
     (flymake-errline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,red)
         :inherit unspecified :foreground unspecified :background unspecified))
       (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     (flymake-infoline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,green)
         :inherit unspecified :foreground unspecified :background unspecified))
       (,class (:foreground ,green-hc :background ,green-lc))))
     (flymake-warnline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,yellow)
         :inherit unspecified :foreground unspecified :background unspecified))
       (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

     ;; flycheck
     (flycheck-error ((,(append '((supports :underline (:style wave))) class)
                       (:underline (:style wave :color ,red) :inherit unspecified))
                      (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     (flycheck-warning ((,(append '((supports :underline (:style wave))) class)
                         (:underline (:style wave :color ,yellow) :inherit unspecified))
                        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     (flycheck-info ((,(append '((supports :underline (:style wave))) class)
                      (:underline (:style wave :color ,blue) :inherit unspecified))
                     (,class (:foreground ,blue-hc :background ,blue-lc :weight bold :underline t))))
     (flycheck-fringe-error ((,class (:foreground ,red-hc :background ,red-lc :weight bold))))
     (flycheck-fringe-warning ((,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold))))
     (flycheck-fringe-info ((,class (:foreground ,blue-hc :background ,blue-lc :weight bold))))

     ;; flyspell
     (flyspell-duplicate
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,yellow) :inherit unspecified))
       (,class (:foreground ,yellow :weight bold :underline t))))
     (flyspell-incorrect
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,red) :inherit unspecified))
       (,class (:foreground ,red :weight bold :underline t))))

     ;; hl-line-mode
     (hl-line ((,class (:background ,monokai-hl-line :inherit t))))
     (hl-line-face ((,class (:background ,monokai-hl-line :inherit t))))

     ;; IDO
     (ido-first-match ((,class (:foreground ,yellow :weight normal))))
     (ido-only-match ((,class (:foreground ,monokai-bg :background ,yellow :weight normal))))
     (ido-subdir ((,class (:foreground ,blue))))
     (ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
     (ido-indicator ((,class (:background ,red :foreground ,monokai-bg :width condensed))))
     (ido-virtual ((,class (:foreground ,cyan))))

     ;; linum-mode
     (linum ((,class (:foreground ,gray-hc :background ,monokai-bg))))

     ;; magit
     (magit-section-title
      ((,class (:foreground ,yellow :weight bold))))
     (magit-branch
      ((,class (:foreground ,orange :weight bold))))
     (magit-item-highlight
      ((,class (:background ,monokai-hl :weight unspecified))))
     (magit-log-author
      ((,class (:foreground ,cyan))))
     (magit-log-graph
      ((,class (:foreground ,monokai-comments))))
     (magit-log-head-label-bisect-bad
      ((,class (:background ,red-hc :foreground ,red-lc :box 1))))
     (magit-log-head-label-bisect-good
      ((,class (:background ,green-hc :foreground ,green-lc :box 1))))
     (magit-log-head-label-default
      ((,class (:background ,monokai-hl :box 1))))
     (magit-log-head-label-local
      ((,class (:background ,blue-lc :foreground ,blue-hc :box 1))))
     (magit-log-head-label-patches
      ((,class (:background ,red-lc :foreground ,red-hc :box 1))))
     (magit-log-head-label-remote
      ((,class (:background ,green-lc :foreground ,green-hc :box 1))))
     (magit-log-head-label-tags
      ((,class (:background ,yellow-lc :foreground ,yellow-hc :box 1))))
     (magit-log-sha1
      ((,class (:foreground ,yellow))))

     ;; markdown-mode
     (markdown-header-face
      ((,class (:foreground ,green))))
     (markdown-header-face-1
      ((,class (:inherit markdown-header-face :height ,monokai-height-plus-4))))
     (markdown-header-face-2
      ((,class (:inherit markdown-header-face :height ,monokai-height-plus-3))))
     (markdown-header-face-3
      ((,class (:inherit markdown-header-face :height ,monokai-height-plus-2))))
     (markdown-header-face-4
      ((,class (:inherit markdown-header-face :height ,monokai-height-plus-1))))
     (markdown-header-face-5
      ((,class (:inherit markdown-header-face))))
     (markdown-header-face-6
      ((,class (:inherit markdown-header-face))))

     ;; org-mode
     (org-agenda-structure
      ((,class (:foreground ,monokai-emph :background ,monokai-hl
                :weight bold :slant normal :inverse-video nil
                :height ,monokai-height-plus-1 :underline nil
                :box (:line-width 2 :color ,monokai-bg)))))
     (org-agenda-calendar-event
      ((,class (:foreground ,monokai-emph))))
     (org-agenda-calendar-sexp
      ((,class (:foreground ,monokai-fg :slant italic))))
     (org-agenda-date
      ((,class (:foreground ,monokai-comments :background ,monokai-bg
                :weight normal :inverse-video nil :overline nil :slant normal :height 1.0
                :box (:line-width 2 :color ,monokai-bg)))) t)
     (org-agenda-date-weekend
      ((,class (:inherit org-agenda-date :inverse-video nil
                :background unspecified :foreground ,monokai-comments
                :weight unspecified :underline t :overline nil :box unspecified))) t)
     (org-agenda-date-today
      ((,class (:inherit org-agenda-date :inverse-video t
                :weight bold :underline unspecified :overline nil :box unspecified
                :foreground ,blue :background ,monokai-bg))) t)
     (org-agenda-done
      ((,class (:foreground ,monokai-comments :slant italic))) t)
     (org-archived
      ((,class (:foreground ,monokai-comments :weight normal))))
     (org-block
      ((,class (:foreground ,monokai-comments))))
     (org-block-begin-line
      ((,class (:foreground ,monokai-comments :slant italic))))
     (org-checkbox
      ((,class (:background ,monokai-bg :foreground ,monokai-fg
                :box (:line-width 1 :style released-button)))))
     (org-code
      ((,class (:foreground ,monokai-comments))))
     (org-date
      ((,class (:foreground ,blue :underline t))))
     (org-done
      ((,class (:weight bold :foreground ,green))))
     (org-ellipsis
      ((,class (:foreground ,monokai-comments))))
     (org-formula
      ((,class (:foreground ,yellow))))
     (org-headline-done
      ((,class (:foreground ,green))))
     (org-hide
      ((,class (:foreground ,monokai-bg))))
     (org-level-1
      ((,class (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-4 :foreground ,orange))))
     (org-level-2
      ((,class (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-3 :foreground ,green))))
     (org-level-3
      ((,class (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-2 :foreground ,blue))))
     (org-level-4
      ((,class (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-1 :foreground ,yellow))))
     (org-level-5
      ((,class (:inherit ,s-variable-pitch :foreground ,cyan))))
     (org-level-6
      ((,class (:inherit ,s-variable-pitch :foreground ,green))))
     (org-level-7
      ((,class (:inherit ,s-variable-pitch :foreground ,red))))
     (org-level-8
      ((,class (:inherit ,s-variable-pitch :foreground ,blue))))
     (org-link
      ((,class (:foreground ,yellow :underline t))))
     (org-sexp-date
      ((,class (:foreground ,violet))))
     (org-scheduled
      ((,class (:foreground ,green))))
     (org-scheduled-previously
      ((,class (:foreground ,cyan))))
     (org-scheduled-today
      ((,class (:foreground ,blue :weight normal))))
     (org-special-keyword
      ((,class (:foreground ,monokai-comments :weight bold))))
     (org-table
      ((,class (:foreground ,green))))
     (org-tag
      ((,class (:weight bold))))
     (org-time-grid
      ((,class (:foreground ,monokai-comments))))
     (org-todo
      ((,class (:foreground ,red :weight bold))))
     (org-upcoming-deadline
      ((,class (:foreground ,yellow :weight normal :underline nil))))
     (org-warning
      ((,class (:foreground ,orange :weight normal :underline nil))))
     (org-agenda-dimmed-todo-face
      ((,class (:foreground ,monokai-comments))))
     (org-agenda-restriction-lock
      ((,class (:background ,yellow))))
     (org-clock-overlay
      ((,class (:background ,yellow))))
     (org-column
      ((,class (:background ,monokai-hl :strike-through nil :underline nil
                :slant normal :weight normal :inherit default))))
     (org-column-title
      ((,class (:background ,monokai-hl :underline t :weight bold))))
     (org-date-selected
      ((,class (:foreground ,red :inverse-video t))))
     (org-document-info
      ((,class (:foreground ,monokai-fg))))
     (org-document-title
      ((,class (:foreground ,monokai-emph
                :weight bold :height ,monokai-height-plus-4))))
     (org-drawer
      ((,class (:foreground ,cyan))))
     (org-footnote
      ((,class (:foreground ,magenta :underline t))))
     (org-latex-and-export-specials
      ((,class (:foreground ,orange))))
     (org-mode-line-clock-overrun
      ((,class (:inherit mode-line :background ,red))))

     ;; SLIME
     (slime-repl-inputed-output-face
      ((,class (:foreground ,red))))

     ;; speedbar
     (speedbar-button-face
      ((,class (:inherit ,s-variable-pitch :foreground ,monokai-comments))))
     (speedbar-directory-face
      ((,class (:inherit ,s-variable-pitch :foreground ,blue))))
     (speedbar-file-face
      ((,class (:inherit ,s-variable-pitch :foreground ,monokai-fg))))
     (speedbar-highlight-face
      ((,class (:inherit ,s-variable-pitch :background ,monokai-hl))))
     (speedbar-selected-face
      ((,class (:inherit ,s-variable-pitch :foreground ,yellow :underline t))))
     (speedbar-separator-face
      ((,class (:inherit ,s-variable-pitch
                :background ,blue
                :foreground ,monokai-bg
                :overline ,cyan))))
     (speedbar-tag-face
      ((,class (:inherit ,s-variable-pitch :foreground ,green))))
     )))

(defun define-monokai-theme ()
  "Define the monokai theme"
  (deftheme monokai "A fruity theme")
  (with-monokai-colors
   'default
   (apply 'custom-theme-set-faces 'monokai (monokai-face-specs)))
  (provide-theme 'monokay))

(defun set-monokai-extra-org-statuses ()
  "Set colors for WORK and WAIT org statuses"
  (with-monokai-colors
        'default
        (setq org-todo-keyword-faces
              `(("WORK" . (:foreground ,yellow :weight bold :box nil))
                ("WAIT" . (:foreground ,orange :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-monokai ()
  "Sets the colors to the monokai theme"
  (interactive)
  (with-monokai-colors
   'default
   (apply 'custom-set-faces (monokai-face-specs))))

(provide 'color-theme-monokai)
;;; color-theme-monokai.el ends there
