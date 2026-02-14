;;; color-theme-catppuccin.el --- Soothing pastel theme for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Credit:
;; This is merely an adaptation of the Catppuccin theme, see
;; https://github.com/catppuccin/emacs
;; Catppuccin copyright notice below:
;;
;; Copyright 2022-present Catppuccin, All rights reserved
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Code:

(require 'org)
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

;;; Theme options
(defgroup exordium-catppuccin nil
  "Catppuccin theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom exordium-catppuccin-highlight-matches t
  "Use background color to make highlighted matches more visible."
  :type 'boolean
  :group 'catppuccin)

(defcustom exordium-catppuccin-italic-comments t
  "Use :slant italic for comments."
  :type 'boolean
  :group 'catppuccin)

(defcustom exordium-catppuccin-italic-blockquotes t
  "Use :slant italic for blockquotes in markdown and org."
  :type 'boolean
  :group 'catppuccin)

(defcustom exordium-catppuccin-dark-line-numbers-background nil
  "Use the mantle background color for line numbers to make them stand out more."
  :type 'boolean
  :group 'catppuccin)

(defcustom exordium-catppuchin-modeline-box t
  "Enable displaying a box around the modeline.
Inspired by the apropospriate-theme."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-catppuccin-flavor 'mocha
  "The flavor to use for the Catppuccin theme.
Must be one of `mocha`, `macchiato`, `frappe`, or `latte`."
  :type '(choice
           (const :tag "Mocha" mocha)
           (const :tag "Macchiato" macchiato)
           (const :tag "Frappe" frappe)
           (const :tag "Latte" latte))
  :group 'catppuccin)

;;; Internal functions.

(defun catppuccin--hex-to-rgb (color)
  "Convert a hex COLOR string like \"#rrggbb\" to a list of three integers."
  (mapcar (lambda (i) (string-to-number (substring color i (+ i 2)) 16))
    '(1 3 5)))

(defun catppuccin--rgb-to-hex (r g b)
  "Convert R, G, B integers to a hex color string."
  (format "#%02x%02x%02x" r g b))

(defun catppuccin--rnd (n)
  "Round N to the nearest integer."
  (round (+ 0.5 n)))

(defun catppuccin-lighten (color value)
  "Lighten COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (catppuccin--rnd
                  (min 255 (+ (* (- 255 v) factor) v))))
        (catppuccin--hex-to-rgb color)))))

(defun catppuccin-darken (color value)
  "Darken COLOR by VALUE% (0–100)."
  (let* ((factor (/ value 100.0)))
    (apply #'catppuccin--rgb-to-hex
      (mapcar (lambda (v)
                (floor (* (- 1 factor) v)))
        (catppuccin--hex-to-rgb color)))))

;;; Color palette.

(defconst catppuccin-colors
  '((latte
     . ((rosewater . "#dc8a78")
        (flamingo  . "#dd7878")
        (pink      . "#ea76cb")
        (mauve     . "#8839ef")
        (red       . "#d20f39")
        (maroon    . "#e64553")
        (peach     . "#fe640b")
        (yellow    . "#df8e1d")
        (green     . "#40a02b")
        (teal      . "#179299")
        (sky       . "#04a5e5")
        (sapphire  . "#209fb5")
        (blue      . "#1e66f5")
        (lavender  . "#7287fd")
        (text      . "#4c4f69")
        (subtext1  . "#5c5f77")
        (subtext0  . "#6c6f85")
        (overlay2  . "#7c7f93")
        (overlay1  . "#8c8fa1")
        (overlay0  . "#9ca0b0")
        (surface2  . "#acb0be")
        (surface1  . "#bcc0cc")
        (surface0  . "#ccd0da")
        (base      . "#eff1f5")
        (mantle    . "#e6e9ef")
        (crust     . "#dce0e8")))
    (frappe
     . ((rosewater . "#f2d5cf")
        (flamingo  . "#eebebe")
        (pink      . "#f4b8e4")
        (mauve     . "#ca9ee6")
        (red       . "#e78284")
        (maroon    . "#ea999c")
        (peach     . "#ef9f76")
        (yellow    . "#e5c890")
        (green     . "#a6d189")
        (teal      . "#81c8be")
        (sky       . "#99d1db")
        (sapphire  . "#85c1dc")
        (blue      . "#8caaee")
        (lavender  . "#babbf1")
        (text      . "#c6d0f5")
        (subtext1  . "#b5bfe2")
        (subtext0  . "#a5adce")
        (overlay2  . "#949cbb")
        (overlay1  . "#838ba7")
        (overlay0  . "#737994")
        (surface2  . "#626880")
        (surface1  . "#51576d")
        (surface0  . "#414559")
        (base      . "#303446")
        (mantle    . "#292c3c")
        (crust     . "#232634")))
    (macchiato
     . ((rosewater . "#f4dbd6")
        (flamingo  . "#f0c6c6")
        (pink      . "#f5bde6")
        (mauve     . "#c6a0f6")
        (red       . "#ed8796")
        (maroon    . "#ee99a0")
        (peach     . "#f5a97f")
        (yellow    . "#eed49f")
        (green     . "#a6da95")
        (teal      . "#8bd5ca")
        (sky       . "#91d7e3")
        (sapphire  . "#7dc4e4")
        (blue      . "#8aadf4")
        (lavender  . "#b7bdf8")
        (text      . "#cad3f5")
        (subtext1  . "#b8c0e0")
        (subtext0  . "#a5adcb")
        (overlay2  . "#939ab7")
        (overlay1  . "#8087a2")
        (overlay0  . "#6e738d")
        (surface2  . "#5b6078")
        (surface1  . "#494d64")
        (surface0  . "#363a4f")
        (base      . "#24273a")
        (mantle    . "#1e2030")
        (crust     . "#181926")))
    (mocha
     . ((rosewater . "#f5e0dc")
        (flamingo  . "#f2cdcd")
        (pink      . "#f5c2e7")
        (mauve     . "#cba6f7")
        (red       . "#f38ba8")
        (maroon    . "#eba0ac")
        (peach     . "#fab387")
        (yellow    . "#f9e2af")
        (green     . "#a6e3a1")
        (teal      . "#94e2d5")
        (sky       . "#89dceb")
        (sapphire  . "#74c7ec")
        (blue      . "#89b4fa")
        (lavender  . "#b4befe")
        (text      . "#cdd6f4")
        (subtext1  . "#bac2de")
        (subtext0  . "#a6adc8")
        (overlay2  . "#9399b2")
        (overlay1  . "#7f849c")
        (overlay0  . "#6c7086")
        (surface2  . "#585b70")
        (surface1  . "#45475a")
        (surface0  . "#313244")
        (base      . "#1e1e2e")
        (mantle    . "#181825")
        (crust     . "#11111b")))))

;;; Theme definition

(defmacro with-catppuccin-colors (mode &rest body)
  "Execute BODY in a scope with variables bound to the various catppuccin colors.
MODE should be set to either \\='mocha, \\='frappe, \\='macchiato,
or \\='latte."
  `(let ((colors (or (cdr (assoc ,mode catppuccin-colors))
                     (error "No such theme flavor"))))
     (let ((rosewater (cdr (assoc 'rosewater colors)))
           (flamingo  (cdr (assoc 'flamingo colors)))
           (pink      (cdr (assoc 'pink colors)))
           (mauve     (cdr (assoc 'mauve colors)))
           (red       (cdr (assoc 'red colors)))
           (maroon    (cdr (assoc 'maroon colors)))
           (peach     (cdr (assoc 'peach colors)))
           (yellow    (cdr (assoc 'yellow colors)))
           (green     (cdr (assoc 'green colors)))
           (teal      (cdr (assoc 'teal colors)))
           (sky       (cdr (assoc 'sky colors)))
           (sapphire  (cdr (assoc 'sapphire colors)))
           (blue      (cdr (assoc 'blue colors)))
           (lavender  (cdr (assoc 'lavender colors)))
           (text      (cdr (assoc 'text colors)))
           (subtext1  (cdr (assoc 'subtext1 colors)))
           (subtext0  (cdr (assoc 'subtext0 colors)))
           (overlay2  (cdr (assoc 'overlay2 colors)))
           (overlay1  (cdr (assoc 'overlay1 colors)))
           (overlay0  (cdr (assoc 'overlay0 colors)))
           (surface2  (cdr (assoc 'surface2 colors)))
           (surface1  (cdr (assoc 'surface1 colors)))
           (surface0  (cdr (assoc 'surface0 colors)))
           (base      (cdr (assoc 'base colors)))
           (mantle    (cdr (assoc 'mantle colors)))
           (crust     (cdr (assoc 'crust colors)))
           (class     '((class color) (min-colors 89))))
       (let ((undef     "#ff00ff")
             (current   (if (eq exordium-catppuccin-flavor 'latte)
                            (catppuccin-darken base 5)
                          (catppuccin-lighten base 5)))
             (selection (if (eq exordium-catppuccin-flavor 'latte)
                            (catppuccin-darken base 12)
                          (catppuccin-lighten base 17)))
             (linum     (if exordium-catppuccin-dark-line-numbers-background
                            mantle
                          base)))
         (ignore class)
         ,@body))))

(defmacro catppuccin-face-specs ()
  "Return a backquote with a list of face specs definitions.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard faces
     (default ((t (:foreground ,text :background ,base))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:slant italic :weight bold))))
     (underline ((t (:underline t))))
     (italic ((t (:slant italic))))
     (shadow ((t (:foreground ,overlay0))))
     (success ((t (:foreground ,green))))
     (error ((t (:foreground ,red))))
     (warning ((t (:foreground ,yellow))))
     (scroll-bar ((t (:background ,base))))
     (cursor ((t (:background ,rosewater))))
     (fringe ((t (:background ,base :foreground ,surface1))))
     (hl-line ((t (:background ,current :inherit nil))))
     (highlight ((t (:background ,current :foreground ,text))))
     (border ((t (:background ,current))))
     (border-glyph ((t (nil))))

     (link ((t (:foreground ,lavender :underline t))))
     (link-unvisited ((t (:foreground ,blue :underline t))))
     (link-visited ((t (:foreground ,lavender))))

     (linum ((t (:inherit default :foreground ,surface1 :background ,linum))))
     (line-number ((t (:inherit default :foreground ,surface1 :background ,linum))))
     (line-number-current-line ((t (:inherit line-number :foreground ,lavender))))

     (widget-button ((t (:underline t))))
     (widget-field ((t (:background ,current :box (:line-width 1 :color ,base)))))
     (header-line ((t (:foreground ,mauve :background nil))))
     (menu ((t (:foreground ,base :background ,selection))))
     (minibuffer-prompt ((t (:weight normal :foreground ,subtext0))))
     (fill-column-indicator ((t (:foreground ,surface2 :slant normal))))

     (tooltip ((t (:foreground ,overlay2 :background ,surface0))))
     (trailing-whitespace ((t (:inherit warning))))
     (window-divider ((t (:foreground ,mantle))))
     (vertical-border ((t (:foreground ,mantle))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((t ,(if exordium-catppuccin-highlight-matches
                                `(:background ,green :foreground ,base :bold t)
                              `(:foreground ,pink :bold t)))))
     (show-paren-match-expression ((t (:inherit match))))
     (show-paren-mismatch ((t :inherit warning :bold t)))

     ;; Region
     (region ((t (:background ,selection :extend t))))
     (secondary-selection ((t (:background ,selection))))

     ;; Search
     (match ((t (:background ,sky :foreground ,base))))
     (isearch ((t (:inherit match :background ,peach))))
     (isearch-fail ((t (:inherit error))))
     (lazy-highlight ((t (:foreground ,subtext1 :background ,surface1))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,yellow :inverse-video t))))
     (hi-pink ((t (:foreground ,mauve :inverse-video t))))
     (hi-green ((t (:foreground ,green :inverse-video t))))
     (hi-blue ((t (:foreground ,teal inverse-video t))))

     ;; Info
     (info-menu-star ((t (:foreground ,red))))
     (info-quoted-name ((t (:foreground ,subtext1))))
     (info-string ((t (:foreground ,green))))

     ;; Font-lock
     (font-lock-bracket-face ((t (:foreground ,overlay2))))
     (font-lock-builtin-face ((t (:foreground ,red))))
     (font-lock-comment-face ((t ,(if exordium-catppuccin-italic-comments
                                      `(:inherit shadow :slant italic)
                                    `(:inherit shadow)))))
     (font-lock-comment-delimiter-face ((t (:inherit shaddow))))
     (font-lock-constant-face ((t (:foreground ,peach))))
     (font-lock-doc-face ((t (:inherit font-lock-comment-face))))
     (font-lock-escape-face ((t (:foreground ,pink))))
     (font-lock-function-call-face ((t (:foreground ,blue))))
     (font-lock-function-name-face ((t (:foreground ,blue))))
     (font-lock-keyword-face ((t (:foreground ,mauve :weight bold))))
     (font-lock-negation-char-face ((t (:foreground ,sky))))
     (font-lock-number-face ((t (:foreground ,peach))))
     (font-lock-operator-face ((t (:foreground ,sky))))
     (font-lock-preprocessor-face ((t (:foreground ,yellow))))
     (font-lock-property-name-face ((t (:foreground ,blue))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,red))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,red))))
     (font-lock-string-face ((t (:foreground ,green))))
     (font-lock-type-face ((t (:foreground ,yellow))))
     (font-lock-variable-name-face ((t (:foreground ,text))))
     (font-lock-variable-use-face ((t (:foreground ,text))))
     (font-lock-warning-face ((t (:weight bold :foreground ,red))))

     ;; ;; Mode line
     (mode-line ((t ,(if exordium-catppuchin-modeline-box
                         `(:box (:line-width 1 :color ,crust :style nil)
                           :height 0.9 :background ,mantle :foreground ,text)
                       `(:background ,mantle :foreground ,text)))))
     (mode-line-inactive ((t ,(if exordium-catppuchin-modeline-box
                                  `(:box (:line-width 1 :color ,crust :style nil)
                                    :height 0.9 :background ,crust :foreground ,overlay0)
                                `(:background ,crust :foreground ,overlay0)))))
     (mode-line-buffer-id ((t (:foreground ,lavender :background nil))))
     (mode-line-emphasis ((t (:foreground ,text :slant italic))))
     (mode-line-highlight ((t (:foreground ,lavender :box nil :weight bold))))
     (minibuffer-prompt ((t (:foreground ,blue))))
     (which-func ((t (:foreground ,overlay0 :weight bold))))

     ;; ;; Powerline
     (exordium-powerline-active1 ((t (:foreground ,peach :background ,mantle))))
     (exordium-powerline-active2 ((t (:foreground ,rosewater :background ,surface0))))
     (exordium-powerline-active3 ((t (:background ,mauve :foreground ,surface0))))
     (exordium-powerline-active4 ((t (:background ,red :foreground ,surface0))))
     (exordium-powerline-active5 ((t (:background ,green :foreground ,surface0))))
     (exordium-powerline-inactive1 ((t (:foreground ,text :background ,mantle))))
     (exordium-powerline-inactive2 ((t (:foreground ,text :background ,surface0))))
     (exordium-powerline-inactive3 ((t (:foreground ,text :background ,surface0))))
     (exordium-project-name ((t (:foreground ,mauve))))

     ;; Dired
     (dired-flagged ((t (:foreground ,maroon :weight bold))))
     (dired-marked ((t (:weight bold))))
     (dired-mark ((t (:inherit dired-marked))))
     (dired-header ((t (:foreground ,sapphire :weight bold))))
     (dired-ignored ((t (:inherit font-lock-comment-face))))
     (dired-special ((t (:foreground ,yellow))))
     (dired-symlink ((t (:foreground ,pink))))
     (dired-warning ((t (:inherit warning))))
     (dired-directory ((t (:foreground ,blue))))
     (dired-perm-write ((t (:foreground ,green))))
     (dired-broken-symlink ((t (:foreground ,text :background ,red))))
     (dired-filetype-common ((t (:foreground ,text))))
     (dired-filetype-compress ((t (:foreground ,yellow))))
     (dired-filetype-document ((t (:foreground ,sky))))
     (dired-filetype-execute ((t (:foreground ,red))))
     (dired-filetype-image ((t (:foreground ,peach))))
     (dired-filetype-js ((t (:foreground ,yellow))))
     (dired-filetype-link ((t (:foreground ,maroon))))
     (dired-filetype-music ((t (:foreground ,maroon))))
     (dired-filetype-omit ((t (:foreground ,mauve))))
     (dired-filetype-plain ((t (:foreground ,text))))
     (dired-filetype-program ((t (:foreground ,peach))))
     (dired-filetype-source ((t (:foreground ,green))))
     (dired-filetype-video ((t (:foreground ,maroon))))
     (dired-filetype-xml ((t (:foreground ,green))))
     ;; diredfl (more modernly published dired+)
     (diredfl-file-name ((t (:inherit dired-file-name))))
     (diredfl-compressed-file-name ((t (:inherit dired-file-name))))
     (diredfl-compressed-file-suffix ((t (:foreground ,green))))
     (diredfl-date-time ((t (:foreground ,subtext0))))
     (diredfl-deletion-file-name ((t (:inherit dired-flagged))))
     (diredfl-deletion ((t (:inherit dired-flagged))))
     (diredfl-dir-heading ((t (:inherit dired-header))))
     (diredfl-dir-name ((t (:inherit dired-directory))))
     (diredfl-dir-priv ((t (:inherit dired-directory))))
     (diredfl-executable-tag ((t (:foreground ,red))))
     (diredfl-file-suffix ((t (:inherit dired-file-name))))
     (diredfl-flag-mark-line ((t (:inherit dired-marked))))
     (diredfl-flag-mark ((t (:inherit dired-mark))))
     (diredfl-ignored-file-name ((t (:foreground ,text))))
     (diredfl-mode-line-flagged ((t (:foreground ,undef))))
     (diredfl-mode-line-marked ((t (:foreground ,undef))))
     (diredfl-no-priv ((t (:foreground ,surface2))))
     (diredfl-number ((t (:foreground ,yellow))))
     (diredfl-other-priv ((t (:inherit diredfl-exec-priv))))
     (diredfl-rare-priv ((t (:inherit diredfl-exec-priv))))
     (diredfl-read-priv ((t (:foreground ,sky))))
     (diredfl-write-priv ((t (:inherit dired-perm-write))))
     (diredfl-exec-priv ((t (:foreground ,red))))
     (diredfl-symlink ((t (:inherit dired-symlink))))
     (diredfl-link-priv ((t (:inherit dired-symlink))))
     (diredfl-autofile-name ((t (:foreground ,undef))))
     (diredfl-tagged-autofile-name ((t (:foreground ,undef))))

     ;; ;; IDO
     (ido-first-match ((t (:foreground ,green))))
     (ido-only-match ((t (:foreground ,green))))
     (ido-subdir ((t (:inherit dired-directory))))
     (ido-virtual ((t (:foreground ,sapphire))))
     (ido-incomplete-regexp ((t (:inherit warning))))
     (ido-indicator ((t (:foreground ,text :weight bold))))

     ;; ;; Helm
     (helm-bookmark-w3m ((t (:foreground ,subtext1))))
     (helm-buffer-not-saved ((t (:foreground ,red))))
     (helm-buffer-process ((t (:foreground ,red))))
     (helm-buffer-saved-out ((t (:foreground ,green))))
     (helm-buffer-size ((t (:foreground ,subtext1))))
     (helm-candidate-number ((t (:foreground ,yellow))))
     (helm-ff-directory ((t (:foreground ,blue))))
     (helm-ff-dotted-directory ((t (:foreground ,blue))))
     (helm-ff-executable ((t (:foreground ,sky))))
     (helm-ff-file ((t (:foreground ,text))))
     (helm-ff-invalid-symlink ((t (:foreground ,red))))
     (helm-ff-prefix ((t (:foreground ,yellow))))
     (helm-ff-symlink ((t (:foreground ,teal))))
     (helm-grep-cmd-line ((t (:foreground ,yellow))))
     (helm-grep-file ((t (:foreground ,red))))
     (helm-grep-finish ((t (:foreground ,red))))
     (helm-grep-lineno ((t (:foreground ,subtext0))))
     (helm-grep-match ((t (:inherit match))))
     (helm-grep-running ((t (:foreground ,undef))))
     (helm-header ((t (:foreground ,subtext1))))
     (helm-moccur-buffer ((t (:foreground ,undef))))
     (helm-selection ((t (:foreground ,peach :background ,surface0))))
     (helm-separator ((t (:foreground ,subtext1))))
     (helm-source-go-package-godoc-description ((t (:foreground ,red))))
     (helm-source-header ((t (:foreground ,subtext1))))
     (helm-time-zone-current ((t (:foreground ,text))))
     (helm-time-zone-home ((t (:foreground ,subtext0))))
     (helm-visible-mark ((t (:foreground ,red))))
     (helm-swoop-target-line-face ((t (:background ,yellow))))
     (helm-swoop-target-word-face ((t (:background ,blue))))

     ;; Diff
     (diff-header ((t (:foreground ,blue))))
     (diff-hunk-header ((t (:foreground ,text :background ,surface2))))
     (diff-added ((t (:background ,(catppuccin-darken green 60)))))
     (diff-removed ((t (:background ,(catppuccin-darken red 60)))))
     (diff-indicator-added ((t (:foreground ,green))))
     (diff-indicator-removed ((t (:foreground ,red))))
     (diff-refine-added ((t (:background ,(catppuccin-darken green 40)))))
     (diff-refine-removed ((t (:background ,(catppuccin-darken red 40)))))
     (diff-refine-changed ((t (:background ,yellow :foreground ,base))))

     ;; ;; Magit
     (magit-branch-local ((t (:foreground ,teal))))
     (magit-branch-remote ((t (:foreground ,green))))
     (magit-tag ((t (:foreground ,peach))))
     (magit-section-heading ((t (:foreground ,blue :weight bold))))
     (magit-section-highlight ((t (:background ,surface0 :extend t))))
     (magit-diff-context-highlight ((t (:background ,surface0 :foreground ,text :extend t))))
     (magit-diff-revision-summary ((t (:foreground ,blue :weight bold))))
     (magit-diff-revision-summary-highlight ((t (:foreground ,blue :weight bold))))
     (magit-diff-added ((t (:foreground ,green :extend t))))
     (magit-diff-added-highlight ((t (:background ,surface1 :foreground ,green :extend t))))
     (magit-diff-removed ((t (:foreground ,red :extend t))))
     (magit-diff-removed-highlight ((t (:background ,surface1 :foreground ,red :extend t))))
     (magit-diff-file-heading ((t (:foreground ,peach :bold t))))
     (magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight))))
     (magit-diffstat-added ((t (:foreground ,green))))
     (magit-diffstat-removed ((t (:foreground ,red))))
     (magit-hash ((t (:foreground ,blue))))
     (magit-diff-hunk-heading ((t (:inherit diff-hunk-header))))
     (magit-diff-hunk-heading-highlight ((t (:inherit diff-hunk-header :weight bold))))
     (magit-log-author ((t (:foreground ,subtext0))))
     (magit-process-ng ((t (:foreground ,peach :weight bold))))
     (magit-process-ok ((t (:foreground ,green :weight bold))))
     (magit-reflog-checkout ((t (:foreground ,green :weight bold))))
     (magit-blame-summary ((t :background ,surface0 :foreground ,green)))
     (magit-blame-date ((t :background ,surface0 :foreground ,green)))
     (magit-blame-hash ((t :background ,surface0 :foreground ,blue)))
     (magit-blame-heading ((t :background ,surface0 :foreground ,peach)))
     (magit-bisect-good ((t (:foreground ,green))))
     (magit-bisect-bad ((t (:foreground ,red))))
     (magit-bisect-skip ((t (:foreground ,yellow))))

     ;; git-gutter
     (git-gutter:modified ((t (:foreground ,peach))))
     (git-gutter:deleted ((t (:foreground ,red))))
     (git-gutter:added ((t :foreground ,green)))
     (git-gutter:seperator ((t :inherit font-lock-comment-face)))
     (git-gutter:unchanged ((t :foreground ,surface0)))

     ;; git-gutter fringe
     (git-gutter-fr:modified ((t (:inherit git-gutter:modified))))
     (git-gutter-fr:deleted ((t (:inherit git-gutter:deleted))))
     (git-gutter-fr:added ((t (:inherit git-gutter:added))))

     ;; Tab-bar
     (tab-bar ((t (:foreground ,subtext0 :background ,base))))
     (tab-bar-tab ((t (:foreground ,text :background ,current))))
     (tab-bar-tab-inactive ((t (:foreground ,subtext0 :background ,base))))
     (tab-line ((t (:inherit tab-bar))))
     (tab-line-tab ((t (:inherit tab-bar-tab))))
     (tab-line-tab-inactive ((t (:inherit tab-bar-tab-inactive))))
     (tab-line-tab-current ((t (:inherit tab-line-tab))))
     (tab-line-highlight ((t (:background ,surface1))))

     ;; Centaur-tabs
     (centaur-tabs-default ((t (:foreground ,subtext0 :background ,base))))
     (centaur-tabs-unselected ((t (:foreground ,subtext0 :background ,mantle))))
     (centaur-tabs-selected ((t (:foreground ,text :background ,current))))
     (centaur-tabs-unselected-modified ((t (:foreground ,maroon :background ,mantle))))
     (centaur-tabs-selected-modified ((t (:foreground ,red :background ,current))))
     (centaur-tabs-close-unselected ((t (:foreground ,subtext0 :background ,mantle))))
     (centaur-tabs-close-selected ((t (:foreground ,text :background ,current))))
     (centaur-tabs-name-mouse-face ((t (:foreground ,text :background ,surface1))))
     (centaur-tabs-close-mouse-face ((t (:foreground ,red :background ,surface1))))
     (centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected-modified))))
     (centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected-modified))))

     ;; Company
     (company-echo-common ((t (:foreground ,base :background ,text))))
     (company-preview ((t (:inherit shadow))))
     (company-preview-common ((t (:inherit company-preview :foreground ,green))))
     (company-preview-search ((t (:inherit company-preview :foreground ,red))))
     (company-tooltip ((t (:inherit tooltip))))
     (company-tooltip-search ((t (:inherit lazy-highlight))))
     (company-tooltip-search-selection ((t (:inherit match))))
     (company-tooltip-selection ((t (:background ,overlay0 :foreground ,text))))
     (company-tooltip-mouse ((t (:background ,base))))
     (company-tooltip-common ((t (:foreground ,text :weight bold))))
     (company-tooltip-common-selection ((t (:foreground ,text :weight bold))))
     (company-tooltip-annotation ((t (:foreground ,green))))
     (company-tooltip-annotation-selection ((t (:foreground ,text))))
     (company-tooltip-scrollbar-thumb ((t (:background ,surface2))))
     (company-tooltip-scrollbar-track ((t (:background ,surface1))))

     ;; Compile
     (compilation-mode-line-exit ((t (:foreground ,green))))
     (compilation-mode-line-fail ((t (:foreground ,red))))
     (compilation-mode-line-number ((t (:foreground ,blue))))

     ;; Completions
     (completions-annotations ((t (:inherit font-lock-comment-face))))
     (completions-common-part ((t (:foreground ,sky))))
     (completions-first-difference ((t (:foreground ,text))))

     ;; Term
     (term ((t (:foreground ,text :background ,base))))
     (term-color-black ((t (:foreground ,surface1 :background ,surface1))))
     (term-color-black-white ((t (:foreground ,surface2 :background ,surface2))))
     (term-color-red ((t (:foreground ,red :background ,red))))
     (term-color-bright-red ((t (:foreground ,red :background ,red))))
     (term-color-green ((t (:foreground ,green :background ,green))))
     (term-color-bright-green ((t (:foreground ,green :background ,green))))
     (term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
     (term-color-bright-yellow ((t (:foreground ,yellow :background ,yellow))))
     (term-color-blue ((t (:foreground ,blue :background ,blue))))
     (term-color-bright-blue ((t (:foreground ,blue :background ,blue))))
     (term-color-magenta ((t (:foreground ,pink :background ,pink))))
     (term-color-bright-magenta ((t (:foreground ,pink :background ,pink))))
     (term-color-cyan ((t (:foreground ,teal :background ,teal))))
     (term-color-bright-cyan ((t (:foreground ,teal :background ,teal))))
     (term-color-white ((t (:foreground ,subtext1 :background ,subtext1))))
     (term-color-bright-white ((t (:foreground ,subtext0 :background ,subtext0))))

     ;; Ansi-color
     (ansi-color-black ((t (:foreground ,surface1))))
     (ansi-color-red ((t (:foreground ,red))))
     (ansi-color-yellow ((t (:foreground ,yellow))))
     (ansi-color-green ((t (:foreground ,green))))
     (ansi-color-blue ((t (:foreground ,blue))))
     (ansi-color-magenta ((t (:foreground ,pink))))
     (ansi-color-cyan ((t (:foreground ,teal))))
     (ansi-color-white ((t (:foreground ,subtext1))))
     (ansi-color-bright-black ((t (:foreground ,surface2))))
     (ansi-color-bright-red ((t (:foreground ,red))))
     (ansi-color-bright-yellow ((t (:foreground ,yellow))))
     (ansi-color-bright-green ((t (:foreground ,green))))
     (ansi-color-bright-blue ((t (:foreground ,blue))))
     (ansi-color-bright-magenta ((t (:foreground ,pink))))
     (ansi-color-bright-cyan ((t (:foreground ,teal))))
     (ansi-color-bright-white ((t (:foreground ,subtext0))))

     ;; Org
     (org-level-1 ((t
                    ,(append `(:foreground ,red
                               :overline nil
                               :inherit nil
                               :extend t)
                             (if exordium-theme-use-big-font `(:height ,exordium-height-plus-4) nil)))))
     (org-level-2 ((t (:foreground ,peach))))
     (org-level-3 ((t (:foreground ,yellow))))
     (org-level-4 ((t (:foreground ,green))))
     (org-level-5 ((t (:foreground ,sapphire))))
     (org-level-6 ((t (:foreground ,lavender))))
     (org-level-7 ((t (:foreground ,mauve))))
     (org-level-8 ((t (:foreground ,maroon))))
     (org-link ((t (:inherit link))))
     (org-agenda-structure ((t (:foreground ,sapphire))))
     (org-agenda-date ((t (:foreground ,subtext0 :underline nil :weight normal))))
     (org-agenda-date-today ((t (:foreground ,subtext0 :underline nil :weight bold))))
     (org-agenda-date-weekend ((t (:inherit org-agenda-date))))
     (org-agenda-date-weekend-today ((t (:inherit org-agenda-date :weight bold))))
     (org-agenda-done ((t (:foreground ,green))))
     (org-agenda-dimmed-todo-face ((t (:inherit font-lock-comment-face))))
     (org-block ((t (:foreground ,green :background ,mantle :extend t))))
     (org-block-begin-line ((t (:inherit org-meta-line :background ,mantle :extend t))))
     (org-block-end-line ((t (:inherit org-block-begin-line :background ,mantle :extend t))))
     (org-code ((t (:foreground ,green))))
     (org-column ((t (:background ,surface0))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground ,subtext0 :underline t))))
     (org-document-info ((t (:foreground ,sapphire))))
     (org-document-info-keyword ((t (:inherit shadow))))
     (org-document-title ((t ,(append `(:weight bold :foreground ,blue)
                                      (if exordium-theme-use-big-font
                                          `(:height ,exordium-height-plus-10)
                                        nil)))))
     (org-todo ((t (:foreground ,peach :weight bold :box nil))))
     (org-done ((t (:foreground ,green :weight bold :box nil))))

     (org-checkbox ((t (:foreground ,yellow :weight bold))))
     (org-ellipsis ((t (:inherit font-lock-comment-face))))
     (org-footnote ((t (:foreground ,mauve))))
     (org-formula ((t (:foreground ,pink))))
     (org-hide ((t (:foreground ,crust :background ,base))))
     (org-link ((t (:foreground ,blue :underline t))))
     (org-scheduled ((t (:foreground ,green))))
     (org-scheduled-previously ((t (:foreground ,teal))))
     (org-scheduled-today ((t (:foreground ,green :weight bold))))
     (org-special-keyword ((t (:inherit font-lock-keyword-face))))
     (org-table ((t (:foreground ,overlay0))))
     (org-upcoming-deadline ((t (:foreground ,maroon))))
     (org-warning ((t (:weight bold :foreground ,red))))
     (exordium-org-wait ((t (:foreground ,yellow :weight bold :box nil))))
     (exordium-org-work ((t (:foreground ,maroon :weight bold :box nil))))
     (exordium-org-stop ((t (:foreground ,blue :weight bold :box nil))))

     ;; Calfw
     (cfw:face-title ((t (:foreground ,blue :weight bold :height 1.5))))
     (cfw:face-header ((t (:foreground ,text))))
     (cfw:face-sunday ((t (:foreground ,overlay1))))
     (cfw:face-saturday ((t (:foreground ,overlay1))))
     (cfw:face-holiday ((t (:foreground ,green))))
     (cfw:face-grid ((t (:foreground ,surface0))))
     (cfw:face-default-content ((t (:foreground ,peach))))
     (cfw:face-periods ((t (:foreground ,undef))))
     (cfw:face-day-title ((t (:foreground ,subtext0))))
     (cfw:face-default-day ((t (:foreground ,text))))
     (cfw:face-annotation ((t (:foreground ,undef))))
     (cfw:face-disable ((t (:foreground ,surface1))))
     (cfw:face-today-title ((t (:foreground ,peach))))
     (cfw:face-today ((t (:inherit cfw:face-today-title))))
     (cfw:face-select ((t (:background ,surface1 :foreground ,text))))
     (cfw:face-toolbar ((t (:background ,base))))
     (cfw:face-toolbar-button-off ((t (:foreground ,lavender))))
     (cfw:face-toolbar-button-on ((t (:foreground ,mauve))))

     ;; ;; Markdown
     (markdown-blockquote-face ((t ,(append `(:extend t :background ,mantle :foreground ,green)
                                            (if exordium-catppuccin-italic-blockquotes
                                                `(:slant italic)
                                              nil)))))
     (markdown-code-face ((t (:foreground ,text))))
     (markdown-footnote-face ((t (:foreground ,yellow))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t ,(append
                                   `(:foreground ,red)
                                   (if exordium-theme-use-big-font
                                       `(:height ,exordium-height-plus-4)
                                     nil)))))
     (markdown-header-face-2 ((t ,(append
                                   `(:foreground ,peach)
                                   (if exordium-theme-use-big-font
                                       `(:height ,exordium-height-plus-2)
                                     nil)))))
     (markdown-header-face-3 ((t (:foreground ,yellow))))
     (markdown-header-face-4 ((t (:foreground ,green))))
     (markdown-header-face-5 ((t (:foreground ,sapphire))))
     (markdown-header-face-6 ((t (:foreground ,lavender))))
     (markdown-inline-code-face ((t (:foreground ,green))))
     (markdown-plain-url-face ((t (:inherit link))))
     (markdown-pre-face ((t (:foreground ,green))))
     (markdown-table-face ((t (:foreground ,text))))
     (markdown-list-face ((t (:foreground ,mauve))))
     (markdown-language-keyword-face ((t (:inherit font-lock-comment-face))))
     (markdown-highlighting-face ((t (:inherit highlight))))
     (markdown-gfm-checkbox-face ((t (:foreground ,yellow))))

     ;; LSP
     (lsp-ui-peek-peek ((t (:background ,mantle))))
     (lsp-ui-peek-list ((t (:background ,mantle))))
     (lsp-ui-peek-filename ((t (:foreground ,text))))
     (lsp-ui-peek-line-number ((t (:inherit default :foreground ,surface1))))
     (lsp-ui-peek-highlight ((t (:inherit highlight :distant-foreground ,base))))
     (lsp-ui-peek-header ((t (:background ,mantle :foreground ,blue :weight bold))))
     (lsp-ui-peek-footer ((t (:inherit lsp-ui-peek-header))))
     (lsp-ui-peek-selection ((t (:inherit match))))
     (lsp-ui-sideline-symbol ((t (:foreground ,subtext0))))
     (lsp-ui-sideline-current-symbol ((t (:foreground ,text :weight bold))))
     (lsp-ui-sideline-code-action ((t (:foreground ,yellow))))
     (lsp-ui-sideline-symbol-info ((t (:slant italic :height 0.99))))
     (lsp-ui-doc-background ((t (:background ,mantle))))
     (lsp-ui-doc-header ((t (:foreground ,sapphire))))

     ;; js2-mode
     (js2-external-variable ((t (:foreground ,red))))
     (js2-function-param ((t (:inherit tree-sitter-hl-face:variable.parameter))))
     (js2-jsdoc-html-tag-delimiter ((t (:inherit web-mode-html-tag-bracket-face))))
     (js2-jsdoc-html-tag-name ((t (:inherit web-mode-html-tag-face))))
     (js2-jsdoc-value ((t (:foreground ,text))))
     (js2-private-function-call ((t (:inherit tree-sitter-hl-face:function.call))))
     (js2-private-member ((t (:inherit font-lock-variable-name-face))))

     ;; js3-mode
     (js3-error-face ((t (:inherit error))))
     (js3-external-variable-face ((t (:foreground ,text))))
     (js3-function-param-face ((t (:inherit js2-function-param))))
     (js3-instance-member-face ((t (:inherit font-lock-variable-name-face))))
     (js3-jsdoc-tag-face ((t (:inherit web-mode-html-tag-face))))
     (js3-warning-face ((t (:inherit warning))))

     ;; flyspell
     (flyspell-duplicate ((t (:underline (:style wave :color ,teal)))))
     (flyspell-incorrect ((t (:underline (:style wave :color ,maroon)))))

     ;; Undo-tree
     (undo-tree-visualizer-current-face :foreground ,peach)
     (undo-tree-visualizer-default-face :foreground ,subtext0)
     (undo-tree-visualizer-register-face :foreground ,mauve)
     (undo-tree-visualizer-unmodified-face :foreground ,text)

     ;; Treemacs
     (treemacs-async-loading-face ((t (:foreground ,text))))
     (treemacs-directory-face ((t (:foreground ,blue))))
     (treemacs-directory-collapsed-face ((t (:foreground ,blue))))
     (treemacs-file-face ((t (:foreground ,text))))
     (treemacs-fringe-indicator-face ((t (:foreground ,text))))
     (treemacs-git-added-face ((t (:foreground ,green))))
     (treemacs-git-commit-diff-face ((t (:foreground ,green))))
     (treemacs-git-conflict-face ((t (:foreground ,red))))
     (treemacs-git-ignored-face ((t (:foreground ,overlay1))))
     (treemacs-git-modified-face ((t (:foreground ,red))))
     (treemacs-git-renamed-face ((t (:foreground ,blue))))
     (treemacs-git-unmodified-face ((t (:foreground ,text))))
     (treemacs-git-untracked-face ((t (:foreground ,green))))
     (treemacs-header-button-face ((t (:foreground ,text))))
     (treemacs-help-column-face ((t (:foreground ,blue))))
     (treemacs-help-title-face ((t (:foreground ,text))))
     (treemacs-hl-line-face ((t (:background ,current :extend t))))
     (treemacs-marked-file-face ((t (:foreground ,red))))
     (treemacs-nerd-icons-face ((t (:foreground ,blue))))
     (treemacs-on-failure-pulse-face ((t (:foreground ,text))))
     (treemacs-on-success-pulse-face ((t (:foreground ,text))))
     (treemacs-peek-mode-indicator-face ((t (:foreground ,text))))
     (treemacs-remote-face ((t (:foreground ,text))))
     (treemacs-root-face ((t (:foreground ,blue :background ,base))))
     (treemacs-root-remote-disconnected-face ((t (:foreground ,yellow))))
     (treemacs-root-remote-unreadable-face ((t (:foreground ,yellow))))
     (treemacs-root-unreadable-face ((t (:foreground ,red))))
     (treemacs-tags-face ((t (:foreground ,text))))
     (treemacs-term-node-face ((t (:foreground ,blue))))
     (treemacs-window-background-face ((t (:background ,base))))
     ;; treemacs-nerd-icons
     (treemacs-nerd-icons-root-face ((t (:foreground ,blue))))
     (treemacs-nerd-icons-file-face ((t (:foreground ,blue))))

     ;; Tree-sitter
     (tree-sitter-hl-face:attribute ((t (:inherit font-lock-constant-face))))
     (tree-sitter-hl-face:property ((t (:foreground ,lavender))))
     (tree-sitter-hl-face:property.definition ((t (:foreground ,lavender))))
     (tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
     (tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
     (tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-builtin-face))))
     (tree-sitter-hl-face:constructor ((t (:inherit font-lock-constant-face))))
     (tree-sitter-hl-face:escape ((t (:foreground ,undef))))
     (tree-sitter-hl-face:function ((t (:inherit font-lock-function-name-face))))
     (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-builtin-face))))
     (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face :weight normal))))
     (tree-sitter-hl-face:function.macro ((t (:inherit font-lock-preprocessor-face))))
     (tree-sitter-hl-face:function.special ((t (:inherit font-lock-preprocessor-face))))
     (tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))
     (tree-sitter-hl-face:punctuation ((t (:foreground ,undef))))
     (tree-sitter-hl-face:punctuation.bracket ((t (:foreground ,overlay2))))
     (tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,overlay2))))
     (tree-sitter-hl-face:punctuation.special ((t (:foreground ,undef))))
     (tree-sitter-hl-face:string ((t (:inherit font-lock-string-face))))
     (tree-sitter-hl-face:string.special ((t (:foreground ,teal))))
     (tree-sitter-hl-face:tag ((t (:inherit font-lock-keyword-face))))
     (tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
     (tree-sitter-hl-face:type.parameter ((t (:foreground ,sapphire))))
     (tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
     (tree-sitter-hl-face:variable.parameter ((t (:foreground ,red))))
     (tree-sitter-hl-face:operator ((t (:foreground ,teal))))

     ;; White space
     (trailing-whitespace ((t (:background ,surface0))))
     (whitespace-big-indent ((t (:foreground ,peach))))
     (whitespace-empty ((t (:inherit warning))))
     (whitespace-hspace ((t (:background ,undef :foreground ,undef))))
     (whitespace-indentation ((t (:foreground ,surface0))))
     (whitespace-line ((t (:underline (:style wave :color ,mauve)))))
     (whitespace-newline ((t (:inherit font-lock-comment-face))))
     (whitespace-space ((t (:inherit font-lock-comment-face))))
     (whitespace-space-after-tab ((t (:inherit warning))))
     (whitespace-space-before-tab ((t (:inherit warning))))
     (whitespace-tab ((t (:inherit whitespace-newline))))
     (whitespace-trailing ((t (:inherit trailing-whitespace))))

     ;; Which-key
     (which-key-key-face ((t (:inherit font-lock-builtin-face))))
     (which-key-command-description-face ((t (:inherit default))))
     (which-key-separator-face ((t (:inherit font-lock-comment-delimiter-face))))
     (which-key-local-map-description-face ((t (:foreground ,green))))

     ;; Lisp programming faces
     (eval-sexp-fu-flash ((t (:background ,peach :foreground ,base))))
     (eval-sexp-fu-flash-error ((t (:background ,red :foreground ,base))))
     (which-func ((t (:inherit font-lock-function-name-face))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,red))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,peach))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,sapphire))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,lavender))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,mauve))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,maroon))))
     (rainbow-delimiters-unmatched-face ((t (:inherit warning))))

     ;; Slime
     (slime-repl-inputed-output-face ((t (:foreground ,mauve))))

     ;; Auctex
     (font-latex-bold-face ((t (:foreground ,red :weight bold))))
     (font-latex-italic-face ((t (:foreground ,yellow :slant italic))))
     (font-latex-match-reference-keywords ((t (:foreground ,teal))))
     (font-latex-match-variable-keywords ((t (:foreground ,text))))
     (font-latex-string-face ((t (:foreground ,green))))
     (font-latex-warning-face ((t (:inherit warning))))

     ;; Other things
     (csv-separator-face ((t (:foreground ,yellow)))))))

(defmacro define-catppuccin-theme (mode)
  "Define a theme for the catppuccin variant `MODE'."
  (let ((name (intern (format "catppuccin-%s" (symbol-name mode))))
        (doc (format "catppuccin-%s" mode)))
    `(progn
       (deftheme ,name ,doc)
       (with-catppuccin-colors
        ',mode
        (apply 'custom-theme-set-faces ',name (catppuccin-face-specs)))
       (provide-theme ',name))))

;;; Extra functions

(defun catppuccin-mode-name ()
  "Return the mode without the catppuccin- prefix, e.g. day, night etc."
  (intern (substring (symbol-name exordium-theme) 9)))

;; Debugging functions

(defun set-colors-catppuccin-mocha ()
  "Set the colors to the catppuccin day theme."
  (interactive)
  (with-catppuccin-colors
   'day
   (apply 'custom-set-faces (catppuccin-face-specs))))

(provide 'color-theme-catppuccin)

;;; color-theme-catppuccin.el ends here
