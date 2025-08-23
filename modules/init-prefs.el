;;; init-prefs.el --- Definition of the variables that prefs.el can override. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; These variables control other init-XXX files, such as init-look-and-feel.el.
;; You can override them by creating your own file ~/.emacs.d/prefs.el
;; with content like:
;;
;; (setq exordium-preferred-frame-width  110
;;       exordium-preferred-frame-height 75)
;;
;; You can also use M-x `customize' and browse group Local -> Init.
;;; Code:

(defgroup exordium nil
  "Customize your Emacs configuration."
  :group 'local)


;;; UI -- see init-look-and-feel.el

(defcustom exordium-preferred-fonts
  '(("Consolas"  . 120)
    ("Monaco"    . 120)
    ("Monospace" . 120)
    ("Mono"      . 120))
  "List of preferred fonts/sizes to use.
The list is in decreasing order of preference.  We will use the
first one that is available on the local machine.  It is an alist
of pairs (FONT-NAME . FONT-SIZE).  If nil, we don't set any font.
Note that you can get the list of available font names by
evaluating `font-family-list'."
  :group 'exordium
  :type  'sexp)

(defcustom exordium-preferred-frame-width 100
  "Default frame width (number of columns)."
  :group 'exordium
  :type  'integer)

;; Note: a weird bug in OS X makes the title bar black if height is too big
(defcustom exordium-preferred-frame-height 60
  "Default frame height (a number of lines)."
  :group 'exordium
  :type  'integer)

(defcustom exordium-split-window-preferred-direction 'vertical
  "The first direction tried when Emacs needs to split a window.
This variable controls in which order
`split-window-sensibly' (which see) will try to split the window.
That order specially matters when both dimensions of the frame
are long enough to be split according to `split-width-threshold'
and `split-height-threshold'.  It is recommended to
experimentally set latter options to values that are compatible
with specific Emacs configuration (as they may differ depending
on frame size (or a screen size if Emacs run in full screen) and
chosen font size.

If the value is `vertical' (the default), `split-window-sensibly'
tries to split vertically first and then horizontally.  If the
value is `horizontal' it does the opposite.  If the value
`longest' the first direction tried depends on the frame shape:
in landscape orientation it will be like `horizontal', but in
portrait it will be like `vertical'.  Basically, the longest of
the two dimension is split first.

If both `split-width-threshold' and `split-height-threshold'
cannot be satisfied, it will fallback to split vertically."
  :group 'exordium
  :type '(radio
          (const :tag "Try to split vertically first"
                 vertical)
          (const :tag "Try to split horizontally first"
                 horizontal)
          (const :tag "Try to split along the longest edge"
                 longest)))

(defcustom exordium-line-mode t
  "Whether the current line is highlighted or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-font-lock t
  "Whether the `font-lock-mode' is turned on globally or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-scroll-bar t
  "Whether a scroll bar is displayed globally or not."
  :group 'exordium
  :type   'boolean)

;;; Keyboard

(defcustom exordium-keyboard-escape nil
  "Whether the Escape key does keyboard quit e.g. the equivalent of Ctrl-G."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-keyboard-ctrl-z-undo t
  "Whether Ctrl-z is rebound to Undo, like most other applications."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-y-or-n t
  "Whether to use \"y\" or  \"n\" answers.
If nil use full words \"yes\" or \"no\"."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-newline-and-indent t
  "Whether to bind RET to `newline-and-indent' and S-RET for just `newline'.
If nil, do the opposite."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-delete-trailing-whitespace t
  "Whether to delete all trailing whitespaces on all lines before saving a buffer.
This is the default because it is generally a good practice.  Set
this variable to to nil if you work on legacy projects that have
a lot of trailing whitespaces."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-electric-pair-mode t
  "Whether to enable `electric-pair-mode'."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-cua-mode nil
  "Whether to enable CUA (`cua-mode').
If set to :region, enables CUA rectangular regions.  If nil,
disables CUA completely."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-enable-evil-mode nil
  "Whether to enable EVIL mode (`evil-mode')."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-insert-gui-primary-selection nil
  "Whether to bind Meta-Insert to `insert-gui-primary-selection'.
This makes it easier to paste text from the Windows clipboard."
  :group 'exordium
  :type  'boolean)

;;; Backup files (e.g. file~)
(defcustom exordium-backup-files nil
  "Whether to enable backup files.
Disabled by default, I can't stand these annoying files~."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-use-magit-fullscreen t
  "Whether `magit-status' and `magit-log' should fill the whole frame.
The original window configuration will be restored when you quit out of magit."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-smerge-show-dispatch t
  "Whether to automatically show `exordium-smerge-dispatch' for unmerged files.
This is when hitting Return on a file with a merge conflict in a
Magit status buffer."
  :group 'exordium
  :type 'boolean)

(defcustom exordium-help-extensions t
  "Whether use help extensions like `which-key' and `helpful-mode'."
  :group 'exordium
  :type 'boolean)


;;; Line numbers - see init-linum.el

(defcustom exordium-display-line-numbers t
  "Whether line numbers are displayed or not.
Set to nil to disable line numbers.  Set to t to enable line numbers.

When buffer matches either of the `exordium-inhibit-line-numbers-modes',
`exordium-inhibit-line-numbers-buffer-size', or
`exordium-inhibit-line-numbers-star-buffers' line numbers WILL NOT be shown."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-inhibit-line-numbers-modes '(Info-mode
                                                 calendar-mode
                                                 compilation-mode
                                                 dired-mode
                                                 eshell-mode
                                                 eww-mode
                                                 help-mode
                                                 image-mode
                                                 iwyu-mode
                                                 magit-mode
                                                 org-mode
                                                 shell-mode
                                                 treemacs-mode)
  "List of modes for which line numbers should not be displayed."
  :group 'exordium
  :type '(repeat symbol))

(defcustom exordium-inhibit-line-numbers-buffer-size nil
  "The maximum buffer size that line numbers should be displayed.
Set to some integer, to show line numbers only for buffers that are smaller
than the specified size.  I.e., `(* 512 1024)' will only display line numbers
that are smaller than 0.5MiB (this is over 6.5k 80 character lines).
Set to nil to enable line numbers even in large buffers."
  :group 'exordium
  :type 'integer)

(defcustom exordium-inhibit-line-numbers-star-buffers nil
  "Whether to display line numbers in buffers that name starts with `*'.
Set to t to don't display line numbers in buffers that name starts with `*'.
Set to nil to display line numbers in buffers that name starts with `*'."
  :group 'exordium
  :type 'boolean)


;;; Miscellaneous utilities -- see init-util.el

(defcustom exordium-fci-mode :ondemand
  "Control fill-column-indicator e.g. the 80-character ruler.
If :ondemand it is off by default (you need to hit C-| to toggle
it).  When :always it is always on.  When :prog it is on for
programming modes.  When nil it is disabled."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-use-dashes :one
  "If non-nil use dashes for fill-column-indicator.
If nil, use a plain line.  The value can be one of `:one', `:two', `:three',
`:four', or any other key from `exordium-fci-dashes-alist' to use associated
value.  Note that the character will only be used if displayable by a current
font."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-dashes-alist '((:one .   (?\u2758 ?\u2575)) ;; ❘ or ╵
                                       (:two .   (?\u254e))         ;; ╎
                                       (:three . (?\u2506))         ;; ┆
                                       (:four .  (?\u250a)))       ;; ┊
  "The mapping form a symbol to a sequence of characters to be used for dashes.
For each symbol the first displayable character from the sequence will be used
if displayable in current configuration.  If the value of
`exordium-fci-use-dashes' cannot be mapped or the mapped value cannot be
displayed by the current font default will be used."
  :group 'exordium
  :type  '(alist :key-type (symbol :tag "Symbol")
                 :value-type (repeat (character :tag "Dashes character"))))

(defcustom exordium-highlight-symbol nil
  "Whether occurrences of symbol under point gets highlighted after a few seconds."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-highlight-symbol-map-modifier 'meta
  "Modifier key for key bindings when point is in a highlighted symbol.
By default `symbol-overlay-map' (which see) defines single letter
bindings.  This conflicts with self-insert commands and
historically it was not Exordium's default.  That is a
highlighted symbol used to be editable by pressing any letter.
Setting this variable to a value of KEY will rebind single
LETTER binding in `symbol-overlay-key' to \"<KEY>-<LETTER>\"
form.  Possible KEY values are:

  * \\='original - use original \"<LETTER>\" bindings, as defined
    in `overlay-symbol-map', which see.

  * \\='meta, \\='control, \\='super, or \\='hyper - use
    \"M-<LETTER>\", \"C-<LETTER>\", \"s-<LETTER>\", or
    \"H-<LETTER>\" respectively."
  :type '(radio (const :tag "Original (see `overlay-symbol-map')" original)
                (const :tag "Meta (`M')" meta)
                (const :tag "Control (`C')" control)
                (const :tag "Super (`s')" super)
                (const :tag "Hyper (`H')" hyper))
  :group 'exordium)

(defcustom exordium-skip-taps-update nil
  "Whether to skip taps update when updating configuration.
If set to nil, each tap need to be updated manually,
i.e., with \"git pull\"."
  :group 'exordium
  :type  'boolean)


;;; Smooth scroll - see init-smooth-scroll.el

(defcustom exordium-smooth-scroll nil
  "Whether smooth scroll is enabled or not."
  :group 'exordium
  :type  'boolean)


;;; Autocomplete -- see init-autocomplete.el

(defcustom exordium-complete-mode :auto-complete
  "Select the completion engine for exordium.
Possible values are `:auto-complete',`:company', and nil.
Default is `:auto-complete'."
  :group 'exordium
  :type  'symbol)


;;; Themes -- see themes directory

(defcustom exordium-theme 'tomorrow-night
  "Theme to load on startup.  Use nil to not load any theme.
Otherwise use one of the following:
- tomorrow-day, tomorrow-night, tomorrow-night-bright,
  tomorrow-night-blue, tomorrow-night-eighties,
- monokai,
- solarized-light, solarized-dark,
- zenburn,
- material,
- atom-one."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-theme-use-big-font t
  "Whether the theme uses big fonts for titles and top-level items.
This applies to modes like `org-mode' or `markdown-mode'.  Set it to
nil if you do not want to have variable-sized font."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-powerline t
  "Whether to enable Powerline."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-display-powerline-after-idle-time 0
  "Fix a race/crash at startup related to Powerline.
The race may happen on some environments, by displaying Powerline
only after a specified number of seconds of idle time.  Normally 1
second is enough."
  :group 'exordium
  :type  'integer)

(defcustom exordium-powerline-theme :angle
  "Control the shape of Powerline separators, and other things.
Possible values are :angle, :wave"
  :group 'exordium
  :type  'symbol)

(defcustom exordium-powerline-enable-icons nil
  "Whether Powerline displays icons for git branches and other things."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-use-variable-pitch nil
  "Allow variable pitch fonts."
  :group 'exordium
  :type 'boolean)

(defcustom exordium-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-2 1.2
  "Font size +2."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-3 1.3
  "Font size +3."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-4 1.4
  "Font size +4."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-10 2.0
  "Font size +10."
  :type 'number
  :group 'exordium)



;;; Programming

;;; See init-prog.el
(defcustom exordium-spell-check :prog
  "Whether to enable `flyspell'.
If set to :prog enable flyspell in `prog-mode' buffers.
If set to nil do not enable `flyspell'."
  :group 'exordium
  :type  'symbol)

;;; See init-yasnippet.el
(defcustom exordium-yasnippet t
  "Whether YASnippet is enabled or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-yasnippet-author
  (format "%s (%s)" user-full-name user-login-name)
  "Author string for yasnippet expansion."
  :group 'exordium
  :type  'string)


;;; See init-projectile.el and init-helm-projectile.el
(defcustom exordium-helm-projectile t
  "Whether Helm and Projectile are enabled or not.
Note that for this variable to have an effect when it is set to
t, the `exordium-projectile' needs to be set to t as well."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-projectile exordium-helm-projectile
  "Whether Projectile is enabled."
  :group 'exordium
  :type 'boolean)

;;; See init-git.el
;;; exordium-git-gutter displays a git diff icon in the left fringe, while
;;; exordium-git-gutter-non-fringe displays the diff icon on the left side of the
;;; line numbers. The latter takes precedence.
(defcustom exordium-git-gutter-non-fringe nil
  "Whether a git status icon is displayed at the very left.
Takes precedence over `exordium-gut-gutter'.  Note that this
option disables highlighting the current line number"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-git-gutter t
  "Whether a git status icon is displayed in the left-side fringe or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-git-gutter-for-remote-files nil
  "Whether a git status icon is displayed for the remote files, i.e.
opened in TRAMP mode."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-git-gutter-fringe-style :flat
  "Style for git gutter fringe markers.
:default = unchanged.
:flat = doom-style"
  :group 'exordium
  :type 'symbol)

;;; See init-cpp.el
(defcustom exordium-enable-c++11-keywords :simple
  "Control syntax highlighting for the new keywords introduced in C++11.
If set to :simple add extra keywords to `font-lock'.
If set to :modern use `modern-cpp-font-lock'."
  :group 'exordium
  :type  'symbol)

;;; See init-clojure.el
(defcustom exordium-clojure nil
  "Whether the development environment for Clojure is enabled or not."
  :group 'exordium
  :type  'boolean)


(defcustom exordium-iwyu-filter-args '("-fno-default-inline")
  "List of flags that should be removed from the \"include-what-you-use\".
When \"include-what-you-use\" is called, all parameters after the
first space in JSON value of `command' property are passed.  Use
this list to remove unsupported flags and arguments as well as
compiler wrappers that are in project's compilation database."
  :group 'exordium
  :type  'sexp)

(defcustom exordium-iwyu-extra-args '()
  "List of extra arguments to call \"include-what-you-use\".
They are passed \\='as is\\=' to \"include-what-you-use\" executable."
  :group 'exordium
  :type  'sexp)


;;; See init-helm.el
(defcustom exordium-helm-everywhere nil
  "Whether Helm should be used as a substitute for common key bindings."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-helm-completion-style 'helm
  "Completion style to be used for Helm."
  :group 'exordium
  :type '(choice (symbol :tag "helm" 'helm)
                 (symbol :tag "helm-fuzzy" 'helm-fuzzy)
                 (symbol :tag "orderless" 'orderless)))

(defcustom exordium-helm-fuzzy-match nil
  "Whether Helm should use fuzzy matching for searches."
  :group 'exordium
  :type  'boolean)
(make-obsolete-variable
 'exordium-helm-fuzzy-match
 "Use 'helm-fuzzy for `exordium-helm-completion-style' instead"
 "20241204"
 'set)

(defcustom exordium-helm-grep-ag-command
  (cond ((executable-find "ag")
         "ag --line-numbers -S --color --color-match '1;34' --nogroup %s -- %s %s")
        (t ;; Fall back to rg, just like `helm-grep' does
         "rg --color=always --colors='match:fg:blue' --smart-case --search-zip --no-heading --line-number %s -- %s %s"))
  "Default value for `helm-grep-ag-command', which see.
Unlike `helm', Exordium prefers to use \"ag\" in case when it is
available, as the \"rg\" is used via package `helm-rg'.  In addition
colors are costomized."
  :group 'exordium
  :type 'string)


;;; Desktop state
(defcustom exordium-desktop nil
  "Whether the desktop state is saved."
  :group 'exordium
  :type  'boolean)


;;; Progress bar
(defcustom exordium-progress-bar nil
  "Whether a progress bar is displayed when Exordium is loaded.
It is disabled by default because it actually makes the
configuration load slower."
  :group 'exordium
  :type  'boolean)


;;; Org mode

(defcustom exordium-enable-org-export t
  "Configure org mode for code export and babel.
Setting this to nil make Emacs starts a little bit faster, if
you don't need to use this feature."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-no-org-babel-confirm nil
  "Disable confirmation requests when evaluating `org-babel' code blocks.
Setting to non-nil could conceivably result in accidentally executing code."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css nil
  "Wether to export from `org-mode' using a css style sheet.
When nil use inline styles.  This allows more control over the
appearance, at the cost of having to maintain a stylesheet."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css-stylesheet ""
  "The stylesheet to use in html based org export.
This will be loaded into `org-html-head' and exported.
For example,

<link rel=\"stylesheet\"
      type=\"text/css\"
      href=\"https://sundev3.dev.bloomberg.com/~sdowney/smd-zenburn.css\" />

To generate a basic css file that matches your theme, use
`org-html-htmlize-generate-css' which will create a buffer with
css definitions for all currently defined faces."
  :group 'exordium
  :type  'string)




;;; LSP
(defun exordium--string-vector-p (candidate)
  "Return non-nil if CANDIDATE is a vector and every element of it a string.
Otherwise return nil."
  (and
   (vectorp candidate)
   (seq-every-p #'stringp candidate)))

(define-widget 'exordium-string-vector 'lazy
  "A vector of zero or more elements, every element of which is a string.
Appropriate for any language-specific `defcustom' that needs to
serialize as a JSON array of strings."
  :offset 4
  :tag "Vector"
  :type '(restricted-sexp
          :match-alternatives (exordium--string-vector-p)))

(defcustom exordium-lsp-clangd-executable ["clangd-18"
                                           "clangd-17"
                                           "clangd-16"
                                           "clangd-15"
                                           "clangd-14"
                                           "clangd-13"
                                           "clangd"]
  "List of executable names to search for to run clangd.
Default is to choose the first that is found via `executable-find'."
  :group 'exordium
  :risky t
  :type 'exordium-string-vector)

(defcustom exordium-lsp-clangd-args '("-j=4"
                                      "--background-index"
                                      "--log=error"
                                      "--clang-tidy")
  "Extra arguments for the clangd executable."
  :group 'exordium
  :risky t
  :type '(repeat string))

(defcustom exordium-enable-which-key t
  "If t, which-key mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-doc-enable t
  "If t, exordium-lsp-ui-doc mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-sideline-enable t
  "If t, exordium-lsp-ui-sideline mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-flycheck-enable t
  "If t, exordium-lsp-ui-flycheck mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-peek-enable t
  "If t, exordium-lsp-ui-peek mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-keymap-prefix "C-c l"
  "The prefix to bind the lsp keymap to, `kbd' format."
  :group 'exordium
  :type  'string)

(defcustom exordium-lsp-ui-doc-position 'at-point
  "Where to display the doc in LSP mode."
  :group 'exordium
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "At point" at-point)))

(defcustom exordium-lsp-ui-flycheck-list-position 'bottom
  "Position where `lsp-ui-flycheck-list' will show diagnostics for the workspace."
  :group 'exordium
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right)))

(defcustom exordium-lsp-mode-enable t
  "Enable lsp-mode."
  :group 'exordium
  :type 'boolean)

(defcustom exordium-treesit-modes-enable nil
  "Enable treesiter.
For Emacs 29 and above enable new -ts-modes and attach forwarding hook."
  :group 'exordium
  :type 'boolean)


(provide 'init-prefs)

;;; init-prefs.el ends here
