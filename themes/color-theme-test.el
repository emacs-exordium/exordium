;;;; Test themes

(defun set-colors-test-solarized-dark (&optional frame)
  "sets the color theme for X-Windows"
  (interactive)
  (let ((color-base03    "#002b36")
        (color-base02    "#073642")
        (color-base01    "#586e75")
        (color-base00    "#657b83")
        (color-base0     "#839496")
        (color-base1     "#93a1a1")
        (color-base2     "#eee8d5")
        (color-base3     "#fdf6e3")
        (color-yellow    "#b58900")
        (color-orange    "#cb4b16")
        (color-red       "#dc322f")
        (color-magenta   "#d33682")
        (color-violet    "#6c71c4")
        (color-blue      "#268bd2")
        (color-cyan      "#2aa198")
        (color-green     "#859900"))
    (if frame
        (select-frame frame)
      (setq frame (selected-frame)))
    (set-background-color color-base03)
    (set-foreground-color color-base1)
    (set-cursor-color color-base3)
    ;; Background
    (set-face-background 'default color-base03)
    (set-face-foreground 'default color-base1)
    ;; Selection
    (set-face-background 'region color-base3)
    ;; Modeline
    (custom-set-faces
     `(mode-line ((t (:foreground ,color-base1 :overline t :underline t)))))
    ;; Trailing whitespace color
    (set-face-background 'trailing-whitespace "#f43841")
    (set-face-foreground 'trailing-whitespace "#000000")
    ;; Current line Highlighting
    (set-face-background 'highlight color-base02)
    ;; Match/Mismatch parenthesis
    (set-face-background 'show-paren-match-face color-base03)
    (set-face-foreground 'show-paren-match-face color-orange)
    (set-face-background 'show-paren-mismatch-face color-red)
    (set-face-foreground 'show-paren-mismatch-face color-base03)
    ;; Other faces
    (set-face-foreground 'font-lock-keyword-face color-violet)
    (set-face-bold-p 'font-lock-keyword-face t)
    (set-face-foreground 'font-lock-function-name-face color-blue)
    (set-face-foreground 'font-lock-constant-face color-cyan)
    (set-face-foreground 'font-lock-string-face color-green)
    (set-face-foreground 'font-lock-type-face color-yellow)
    (set-face-foreground 'font-lock-comment-face color-base01)
    (set-face-italic-p 'font-lock-comment-face t)
    (set-face-foreground 'font-lock-doc-face color-base01)
    (set-face-italic-p 'font-lock-doc-face t)
    (set-face-foreground 'font-lock-preprocessor-face color-red)))

;; Night theme
;; Inspiration: Monokai and Tomorrow Night themes
(defun set-colors-test-tomorrow-night (&optional frame)
  "sets the color theme for X-Windows"
  (interactive)
  (let ((color-black    "#111111")  ; "#222222"
        (color-white    "#e4e4ef")
        (color-default  "LightGray")
        (color-comment  "Gray50")   ; "#969896" or "#75715e" = dark aluminum
        (color-red      "#d54e53")
        (color-orange   "#e78c45")
        (color-yellow   "#e7c547")
        (color-green    "#b9ca4a")  ; "#a6e22e"
        (color-aqua     "#70c0b1")
        (color-blue     "#7aa6da")  ; "LightSkyBlue"
        (color-purple   "#c397d8")  ; "violet"
        (color-error    "DarkRed"))
    (if frame
        (select-frame frame)
      (setq frame (selected-frame)))
    (set-background-color color-black)
    (set-foreground-color color-white)
    (set-mouse-color "#ffdd33")
    (set-cursor-color color-white)
    ;; Background
    (set-face-background 'default color-black)
    (set-face-foreground 'default color-default)
    ;; Selection
    (set-face-background 'region "#484848")
    ;; Modeline
    (custom-set-faces
     `(mode-line ((t (:foreground ,color-white :overline t :underline t)))))
    ;; Trailing whitespace color
    (set-face-background 'trailing-whitespace "#f43841")
    (set-face-foreground 'trailing-whitespace "#000000")
    ;; Current line Highlighting
    (set-face-background 'highlight "#282828")
    ;; Match/Mismatch parenthesis
    (set-face-background 'show-paren-match-face color-black)
    (set-face-foreground 'show-paren-match-face color-orange)
    (set-face-background 'show-paren-mismatch-face color-red)
    (set-face-foreground 'show-paren-mismatch-face color-error)
    ;; Other faces
    (set-face-foreground 'font-lock-keyword-face color-purple)
    (set-face-bold-p 'font-lock-keyword-face t)
    (set-face-foreground 'font-lock-function-name-face color-blue)
    (set-face-foreground 'font-lock-constant-face color-aqua)
    ;;(set-face-foreground 'font-lock-builtin-face color-yellow)
    (set-face-foreground 'font-lock-string-face color-green)
    (set-face-foreground 'font-lock-type-face color-yellow)
    (set-face-foreground 'font-lock-comment-face color-comment)
    (set-face-italic-p 'font-lock-comment-face t)
    (set-face-foreground 'font-lock-doc-face color-comment)
    (set-face-italic-p 'font-lock-doc-face t)
    (set-face-foreground 'font-lock-preprocessor-face color-red)))

(defun set-colors-test-tomorrow-day (&optional frame)
  "sets the color theme for X-Windows"
  (interactive)

  (let ((color-background   "#ffffff")
        (color-current-line "#efefef")
        (color-selection    "#d6d6d6")
        (color-foreground   "#4d4d4c")
        (color-comment      "#8e908c")
        (color-red          "#c82829")
        (color-orange       "#f5871f")
        (color-yellow       "#eab700")
        (color-green        "#718c00")
        (color-aqua         "#3e999f")
        (color-blue         "#4271ae")
        (color-purple       "#8959a8"))
    (if frame
        (select-frame frame)
      (setq frame (selected-frame)))
    (set-background-color color-background)
    (set-foreground-color color-foreground)
    ;;(set-mouse-color "#ffdd33")
    (set-cursor-color color-foreground)
    ;; Background
    (set-face-background 'default color-background)
    (set-face-foreground 'default color-foreground)
    ;; Selection
    (set-face-background 'region color-selection)
    ;; Modeline
    (custom-set-faces
     `(mode-line ((t (:foreground ,color-background :overline t :underline t)))))
    ;; Trailing whitespace color
    (set-face-background 'trailing-whitespace "#f43841")
    (set-face-foreground 'trailing-whitespace "#000000")
    ;; Current line Highlighting
    (set-face-background 'highlight color-current-line)
    ;; Match/Mismatch parenthesis
    (set-face-background 'show-paren-match-face color-background)
    (set-face-foreground 'show-paren-match-face color-orange)
    (set-face-background 'show-paren-mismatch-face color-red)
    (set-face-foreground 'show-paren-mismatch-face color-foreground)
    ;; Other faces
    (set-face-foreground 'font-lock-keyword-face color-purple)
    (set-face-bold-p 'font-lock-keyword-face t)
    (set-face-foreground 'font-lock-type-face color-foreground)
    (set-face-foreground 'font-lock-function-name-face color-blue)
    (set-face-foreground 'font-lock-constant-face color-aqua)
    (set-face-foreground 'font-lock-variable-name-face color-foreground)
    (set-face-foreground 'font-lock-builtin-face color-foreground)
    (set-face-foreground 'font-lock-string-face color-green)
    (set-face-foreground 'font-lock-comment-face color-comment)
    (set-face-italic-p 'font-lock-comment-face t)
    (set-face-foreground 'font-lock-doc-face color-comment)
    (set-face-italic-p 'font-lock-doc-face t)
    (set-face-foreground 'font-lock-preprocessor-face color-red)))

(provide 'color-theme-test)
