;;; color-theme-monokai.el --- a fruity theme.
;;;
;;; Credit:
;;; Inspired by the "Monokai" theme from Kelvin Smith.

(require 'org)
(require 'init-prefs)

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
     (default ((t (:foreground ,monokai-fg :background ,monokai-bg))))
     (shadow ((t (:background ,monokai-hl-line))))
     (match ((t (:background ,monokai-hl :foreground ,monokai-emph :weight bold))))
     (cursor ((t (:foreground ,monokai-bg :background ,monokai-fg :inverse-video t))))
     (mouse ((t (:foreground ,monokai-bg :background ,monokai-fg :inverse-video t))))
     (escape-glyph-face ((t (:foreground ,red))))
     (fringe ((t (:foreground ,monokai-fg :background ,monokai-bg))))
     (highlight ((t (:background ,monokai-hl))))
     (link ((t (:foreground ,blue :underline t :weight bold))))
     (link-visited ((t (:foreground ,blue :underline t :weight normal))))
     (success ((t (:foreground ,green ))))
     (warning ((t (:foreground ,yellow ))))
     (error ((t (:foreground ,orange))))
     (lazy-highlight ((t (:foreground ,monokai-bg :background ,yellow :weight normal))))
     (escape-glyph ((t (:foreground ,violet))))
     (button ((t (:underline t))))
     (scroll-bar ((t (:background ,monokai-bg))))

     ;; font lock
     (font-lock-builtin-face ((t (:foreground ,red :weight normal))))
     (font-lock-comment-delimiter-face ((t (:foreground ,monokai-comments :slant italic))))
     (font-lock-comment-face ((t (:foreground ,monokai-comments :slant italic))))
     (font-lock-constant-face ((t (:foreground ,violet))))
     (font-lock-doc-face ((t (:foreground ,monokai-comments :slant italic))))
     (font-lock-function-name-face ((t (:foreground ,green))))
     (font-lock-keyword-face ((t (:foreground ,red :weight normal))))
     (font-lock-negation-char-face ((t (:foreground ,yellow :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground ,red))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight normal))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,violet :weight normal))))
     (font-lock-string-face ((t (:foreground ,yellow))))
     (font-lock-type-face ((t (:foreground ,blue :italic nil))))
     (font-lock-variable-name-face ((t (:foreground ,monokai-fg))))
     (font-lock-warning-face ((t (:foreground ,orange :weight bold :slant italic :underline t))))
     (c-annotation-face ((t (:inherit font-lock-constant-face))))

     ;; Mode line
     (mode-line ((t (:inverse-video unspecified
                          :underline unspecified
                          :foreground ,monokai-fg
                          :background ,monokai-hl ; for powerline (previously monokai-hl-line)
                          :box (:line-width 1 :color ,monokai-hl-line :style unspecified)))))
     (mode-line-buffer-id ((t (:foreground ,green :weight bold))))
     (mode-line-inactive ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,monokai-comments
                                   :background ,monokai-hl-line ; for powerline (previously monokai-bg)
                                   :box (:line-width 1 :color ,monokai-hl-line :style unspecified)))))
     (which-func ((t (:foreground ,monokai-bg :weight bold))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,monokai-hl))))
     (exordium-powerline-active2 ((t (:background ,monokai-hl-line))))
     (exordium-powerline-active3 ((t (:background ,violet :foreground ,monokai-bg))))
     (exordium-powerline-active4 ((t (:background ,red :foreground ,monokai-bg))))
     (exordium-powerline-active5 ((t (:background ,green :foreground ,monokai-bg))))
     (exordium-powerline-inactive1 ((t (:background ,monokai-hl-line))))
     (exordium-powerline-inactive2 ((t (:background ,monokai-bg))))
     (exordium-powerline-inactive3 ((t (:background ,monokai-comments :foreground ,monokai-bg))))
     (exordium-project-name ((t (:foreground ,violet))))

     ;; compilation
     (compilation-column-face ((t (:foreground ,cyan :underline nil))))
     (compilation-column-number ((t (:inherit font-lock-doc-face
                                          :foreground ,cyan :underline nil))))
     (compilation-enter-directory-face ((t (:foreground ,green :underline nil))))
     (compilation-error ((t (:inherit error :underline nil))))
     (compilation-error-face ((t (:foreground ,red :underline nil))))
     (compilation-face ((t (:foreground ,monokai-fg :underline nil))))
     (compilation-info ((t (:foreground ,monokai-comments :underline nil :bold nil))))
     (compilation-info-face ((t (:foreground ,blue :underline nil))))
     (compilation-leave-directory-face ((t (:foreground ,green :underline nil))))
     (compilation-line-face ((t (:foreground ,green :underline nil))))
     (compilation-line-number ((t (:foreground ,green :underline nil))))
     (compilation-warning ((t (:inherit warning :underline nil))))
     (compilation-warning-face ((t (:foreground ,yellow :weight normal :underline nil))))
     (compilation-mode-line-exit ((t (:inherit compilation-info
                                           :foreground ,green :weight bold))))
     (compilation-mode-line-fail ((t (:inherit compilation-error
                                           :foreground ,red :weight bold))))
     (compilation-mode-line-run ((t (:foreground ,orange :weight bold))))

     ;; RTags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,red :foreground ,monokai-bg)
                                `(:underline (:color ,red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,orange :foreground ,monokai-bg)
                                 `(:underline (:color ,orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,green :foreground ,monokai-bg)
                                  `(:underline (:color ,green :style wave))))))
     (rtags-skippedline ((t (:background ,monokai-hl-line))))

     ;; cua
     (cua-global-mark ((t (:background ,yellow :foreground ,monokai-bg))))
     (cua-rectangle ((t (:inherit region :background ,magenta :foreground ,monokai-bg))))
     (cua-rectangle-noselect ((t (:inherit region
                                       :background ,monokai-hl :foreground ,monokai-comments))))

     ;; dired
     (dired-directory ((t (:foreground ,blue :weight normal))))
     (dired-flagged ((t (:foreground ,red))))
     (dired-header ((t (:foreground ,monokai-bg :background ,blue))))
     (dired-ignored ((t (:inherit shadow))))
     (dired-mark ((t (:foreground ,yellow :weight bold))))
     (dired-marked ((t (:foreground ,magenta :weight bold))))
     (dired-perm-write ((t (:foreground ,monokai-fg :underline t))))
     (dired-symlink ((t (:foreground ,cyan :weight normal :slant italic))))
     (dired-warning ((t (:foreground ,orange :underline t))))

     ;; grep
     (grep-context-face ((t (:foreground ,monokai-fg))))
     (grep-error-face ((t (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,blue))))
     (grep-match-face ((t (:foreground ,orange :weight bold))))

     ;; faces used by isearch
     (isearch ((t (:foreground ,monokai-bg :background ,magenta :weight normal))))
     (isearch-fail ((t (:foreground ,red :background ,monokai-bg :bold t))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,yellow :background ,monokai-bg :inverse-video t))))
     (hi-pink ((t (:foreground ,violet :background ,monokai-bg :inverse-video t))))
     (hi-green ((t (:foreground ,green :background ,monokai-bg :inverse-video t))))
     (hi-blue ((t (:foreground ,cyan :background ,monokai-bg :inverse-video t))))

     ;; misc faces
     (menu ((t (:foreground ,monokai-fg :background ,monokai-bg))))
     (minibuffer-prompt ((t (:foreground ,blue))))

     (header-line ((t (:inverse-video unspecified
                            :underline unspecified
                            :foreground ,monokai-emph
                            :background ,monokai-hl
                            :box (:line-width 1 :color ,monokai-hl :style unspecified)))))
     (region ((t (:background ,monokai-hl :inherit t))))
     (secondary-selection ((t (:background ,monokai-hl :inherit t))))
     (trailing-whitespace ((t (:background ,red))))
     (vertical-border ((t (:foreground ,monokai-hl))))

     ;; auto-complete
     (ac-candidate-face ((t (:background ,monokai-hl :foreground ,cyan))))
     (ac-selection-face ((t (:background ,cyan-lc :foreground ,cyan-hc))))
     (ac-candidate-mouse-face ((t (:background ,cyan-hc :foreground ,cyan-lc))))
     (ac-completion-face ((t (:foreground ,monokai-emph :underline t))))
     (ac-gtags-candidate-face ((t (:background ,monokai-hl :foreground ,blue))))
     (ac-gtags-selection-face ((t (:background ,blue-lc :foreground ,blue-hc))))
     (ac-yasnippet-candidate-face ((t (:background ,monokai-hl :foreground ,yellow))))
     (ac-yasnippet-selection-face ((t (:background ,yellow-lc :foreground ,yellow-hc))))

     ;; clojure-test-mode
     (clojure-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     (clojure-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     (clojure-test-success-face ((t (:foreground ,green :weight bold :underline t))))

     ;; diff
     (diff-added ((t (:foreground ,green :background ,monokai-bg))))
     (diff-changed ((t (:foreground ,blue :background ,monokai-bg))))
     (diff-removed ((t (:foreground ,red :background ,monokai-bg))))
     (diff-header ((t (:background ,monokai-bg))))
     (diff-file-header ((t (:background ,monokai-bg :foreground ,monokai-fg :weight bold))))
     (diff-refine-added ((t :foreground ,monokai-bg :background ,green)))
     (diff-refine-change ((t :foreground ,monokai-bg :background ,blue)))
     (diff-refine-removed ((t (:foreground ,monokai-bg :background ,red))))

     ;; ediff
     (ediff-fine-diff-A ((t (:background ,orange-lc))))
     (ediff-fine-diff-B ((t (:background ,green-lc))))
     (ediff-fine-diff-C ((t (:background ,yellow-lc))))
     (ediff-current-diff-C ((t (:background ,blue-lc))))
     (ediff-even-diff-A ((t (:background ,monokai-comments :foreground ,monokai-fg-lc ))))
     (ediff-odd-diff-A ((t (:background ,monokai-comments :foreground ,monokai-fg-hc ))))
     (ediff-even-diff-B ((t (:background ,monokai-comments :foreground ,monokai-fg-hc ))))
     (ediff-odd-diff-B ((t (:background ,monokai-comments :foreground ,monokai-fg-lc ))))
     (ediff-even-diff-C ((t (:background ,monokai-comments :foreground ,monokai-fg ))))
     (ediff-odd-diff-C ((t (:background ,monokai-comments :foreground ,monokai-bg ))))

     ;; js2-mode
     (js2-warning ((t (:underline ,orange :style wave))))
     (js2-error ((t (:foreground nil :underline ,red :style wave))))
     (js2-external-variable ((t (:foreground ,violet))))
     (js2-function-param ((t (:foreground ,blue))))
     (js2-instance-member ((t (:foreground ,blue))))
     (js2-private-function-call ((t (:foreground ,red))))

     ;; EnhRuby-mode
     (erm-syn-warnline ((t (:underline (:color ,orange :style wave)))))
     (erm-syn-errline ((t (:underline (:color ,red :style wave)))))

     ;; flymake
     (flymake-errline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,red)
         :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     (flymake-infoline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,green)
         :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,green-hc :background ,green-lc))))
     (flymake-warnline
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,yellow)
         :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

     ;; flycheck
     (flycheck-error ((,(append '((supports :underline (:style wave))) class)
                       (:underline (:style wave :color ,red) :inherit unspecified))
                      (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     (flycheck-warning ((,(append '((supports :underline (:style wave))) class)
                         (:underline (:style wave :color ,yellow) :inherit unspecified))
                        (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     (flycheck-info ((,(append '((supports :underline (:style wave))) class)
                      (:underline (:style wave :color ,blue) :inherit unspecified))
                     (t (:foreground ,blue-hc :background ,blue-lc :weight bold :underline t))))
     (flycheck-fringe-error ((t (:foreground ,red-hc :background ,red-lc :weight bold))))
     (flycheck-fringe-warning ((t (:foreground ,yellow-hc :background ,yellow-lc :weight bold))))
     (flycheck-fringe-info ((t (:foreground ,blue-hc :background ,blue-lc :weight bold))))

     ;; flyspell
     (flyspell-duplicate
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,yellow) :inherit unspecified))
       (t (:foreground ,yellow :weight bold :underline t))))
     (flyspell-incorrect
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,red) :inherit unspecified))
       (t (:foreground ,red :weight bold :underline t))))

     ;; hl-line-mode
     (hl-line ((t (:background ,monokai-hl-line :inherit t))))
     (hl-line-face ((t (:background ,monokai-hl-line :inherit t))))

     ;; IDO
     (ido-first-match ((t (:foreground ,yellow :weight normal))))
     (ido-only-match ((t (:foreground ,monokai-bg :background ,yellow :weight normal))))
     (ido-subdir ((t (:foreground ,blue))))
     (ido-incomplete-regexp ((t (:foreground ,red :weight bold ))))
     (ido-indicator ((t (:background ,red :foreground ,monokai-bg :width condensed))))
     (ido-virtual ((t (:foreground ,cyan))))

     ;; linum-mode
     (linum ((t (:foreground ,gray-hc :background ,monokai-bg))))

     ;; magit
     (magit-section-title
      ((t (:foreground ,yellow :weight bold))))
     (magit-branch
      ((t (:foreground ,orange :weight bold))))
     (magit-item-highlight
      ((t (:background ,monokai-hl :weight unspecified))))
     (magit-log-author
      ((t (:foreground ,cyan))))
     (magit-log-graph
      ((t (:foreground ,monokai-comments))))
     (magit-log-head-label-bisect-bad
      ((t (:background ,red-hc :foreground ,red-lc :box 1))))
     (magit-log-head-label-bisect-good
      ((t (:background ,green-hc :foreground ,green-lc :box 1))))
     (magit-log-head-label-default
      ((t (:background ,monokai-hl :box 1))))
     (magit-log-head-label-local
      ((t (:background ,blue-lc :foreground ,blue-hc :box 1))))
     (magit-log-head-label-patches
      ((t (:background ,red-lc :foreground ,red-hc :box 1))))
     (magit-log-head-label-remote
      ((t (:background ,green-lc :foreground ,green-hc :box 1))))
     (magit-log-head-label-tags
      ((t (:background ,yellow-lc :foreground ,yellow-hc :box 1))))
     (magit-log-sha1
      ((t (:foreground ,yellow))))

     ;; markdown-mode
     (markdown-header-face
      ((t (:foreground ,green))))
     (markdown-header-face-1
      ((t (:inherit markdown-header-face :height ,monokai-height-plus-4))))
     (markdown-header-face-2
      ((t (:inherit markdown-header-face :height ,monokai-height-plus-3))))
     (markdown-header-face-3
      ((t (:inherit markdown-header-face :height ,monokai-height-plus-2))))
     (markdown-header-face-4
      ((t (:inherit markdown-header-face :height ,monokai-height-plus-1))))
     (markdown-header-face-5
      ((t (:inherit markdown-header-face))))
     (markdown-header-face-6
      ((t (:inherit markdown-header-face))))

     ;; org-mode
     (org-agenda-structure
      ((t (:foreground ,monokai-emph :background ,monokai-hl
                :weight bold :slant normal :inverse-video nil
                :height ,monokai-height-plus-1 :underline nil
                :box (:line-width 2 :color ,monokai-bg)))))
     (org-agenda-calendar-event
      ((t (:foreground ,monokai-emph))))
     (org-agenda-calendar-sexp
      ((t (:foreground ,monokai-fg :slant italic))))
     (org-agenda-date
      ((t (:foreground ,monokai-comments :background ,monokai-bg
                :weight normal :inverse-video nil :overline nil :slant normal :height 1.0
                :box (:line-width 2 :color ,monokai-bg)))) t)
     (org-agenda-date-weekend
      ((t (:inherit org-agenda-date :inverse-video nil
                :background unspecified :foreground ,monokai-comments
                :weight unspecified :underline t :overline nil :box unspecified))) t)
     (org-agenda-date-today
      ((t (:inherit org-agenda-date :inverse-video t
                :weight bold :underline unspecified :overline nil :box unspecified
                :foreground ,blue :background ,monokai-bg))) t)
     (org-agenda-done
      ((t (:foreground ,monokai-comments :slant italic))) t)
     (org-archived
      ((t (:foreground ,monokai-comments :weight normal))))
     (org-block
      ((t (:foreground ,monokai-comments))))
     (org-block-begin-line
      ((t (:foreground ,monokai-comments :slant italic))))
     (org-checkbox
      ((t (:background ,monokai-bg :foreground ,monokai-fg
                :box (:line-width 1 :style released-button)))))
     (org-code
      ((t (:foreground ,monokai-comments))))
     (org-date
      ((t (:foreground ,blue :underline t))))
     (org-done
      ((t (:weight bold :foreground ,green))))
     (org-ellipsis
      ((t (:foreground ,monokai-comments))))
     (org-formula
      ((t (:foreground ,yellow))))
     (org-headline-done
      ((t (:foreground ,green))))
     (org-hide
      ((t (:foreground ,monokai-bg))))
     (org-level-1
      ((t (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-4 :foreground ,orange))))
     (org-level-2
      ((t (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-3 :foreground ,green))))
     (org-level-3
      ((t (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-2 :foreground ,blue))))
     (org-level-4
      ((t (:inherit ,s-variable-pitch
                :height ,monokai-height-plus-1 :foreground ,yellow))))
     (org-level-5
      ((t (:inherit ,s-variable-pitch :foreground ,cyan))))
     (org-level-6
      ((t (:inherit ,s-variable-pitch :foreground ,green))))
     (org-level-7
      ((t (:inherit ,s-variable-pitch :foreground ,red))))
     (org-level-8
      ((t (:inherit ,s-variable-pitch :foreground ,blue))))
     (org-link
      ((t (:foreground ,yellow :underline t))))
     (org-sexp-date
      ((t (:foreground ,violet))))
     (org-scheduled
      ((t (:foreground ,green))))
     (org-scheduled-previously
      ((t (:foreground ,cyan))))
     (org-scheduled-today
      ((t (:foreground ,blue :weight normal))))
     (org-special-keyword
      ((t (:foreground ,monokai-comments :weight bold))))
     (org-table
      ((t (:foreground ,green))))
     (org-tag
      ((t (:weight bold))))
     (org-time-grid
      ((t (:foreground ,monokai-comments))))
     (org-todo
      ((t (:foreground ,red :weight bold))))
     (org-upcoming-deadline
      ((t (:foreground ,yellow :weight normal :underline nil))))
     (org-warning
      ((t (:foreground ,orange :weight normal :underline nil))))
     (org-agenda-dimmed-todo-face
      ((t (:foreground ,monokai-comments))))
     (org-agenda-restriction-lock
      ((t (:background ,yellow))))
     (org-clock-overlay
      ((t (:background ,yellow))))
     (org-column
      ((t (:background ,monokai-hl :strike-through nil :underline nil
                :slant normal :weight normal :inherit default))))
     (org-column-title
      ((t (:background ,monokai-hl :underline t :weight bold))))
     (org-date-selected
      ((t (:foreground ,red :inverse-video t))))
     (org-document-info
      ((t (:foreground ,monokai-fg))))
     (org-document-title
      ((t (:foreground ,monokai-emph
                :weight bold :height ,monokai-height-plus-4))))
     (org-drawer
      ((t (:foreground ,cyan))))
     (org-footnote
      ((t (:foreground ,magenta :underline t))))
     (org-latex-and-export-specials
      ((t (:foreground ,orange))))
     (org-mode-line-clock-overrun
      ((t (:inherit mode-line :background ,red))))

     ;; SLIME
     (slime-repl-inputed-output-face
      ((t (:foreground ,red))))

     ;; Emacs Lisp
     (eval-sexp-fu-flash ((t (:background ,orange :foreground ,monokai-bg))))
     (eval-sexp-fu-flash-error ((t (:background ,red :foreground ,monokai-bg))))

     ;; speedbar
     (speedbar-button-face
      ((t (:inherit ,s-variable-pitch :foreground ,monokai-comments))))
     (speedbar-directory-face
      ((t (:inherit ,s-variable-pitch :foreground ,blue))))
     (speedbar-file-face
      ((t (:inherit ,s-variable-pitch :foreground ,monokai-fg))))
     (speedbar-highlight-face
      ((t (:inherit ,s-variable-pitch :background ,monokai-hl))))
     (speedbar-selected-face
      ((t (:inherit ,s-variable-pitch :foreground ,yellow :underline t))))
     (speedbar-separator-face
      ((t (:inherit ,s-variable-pitch
                :background ,blue
                :foreground ,monokai-bg
                :overline ,cyan))))
     (speedbar-tag-face
      ((t (:inherit ,s-variable-pitch :foreground ,green))))
     )))

(defun define-monokai-theme ()
  "Define the monokai theme"
  (deftheme monokai "A fruity theme")
  (with-monokai-colors
   'default
   (apply 'custom-theme-set-faces 'monokai (monokai-face-specs)))
  (provide-theme 'monokai))

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
