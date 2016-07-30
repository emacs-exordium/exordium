;;;; Highlighting things.
;;;
;;; This module controls the highlighting of the current line and of the symbol
;;; under point.
;;;
;;; If `exordium-line-mode' is t, the line under the cursor is highlighted.
;;;
;;; If `exordium-highlight-symbol' is t, the symbol under the cursor is
;;; highlighted after a small delay using a dim background.
;;;
;;; Alternatively, one can use the key C-c C-SPACE to highlight/un-highlight the
;;; symbol under the cursor in the current buffer. Up to 4 different symbols
;;; may be highlighted at one time, using different colors. Feel free to rebind
;;; function `exordium-highlight-symbol' to a better key, like for example:
;;;
;;; (global-set-key [(control return)] #'exordium-highlight-symbol)
;;; (global-set-key [(f6)] #'exordium-highlight-symbol)
;;;
;;; Notes: Exordium uses 2 packages for highlighting the symbol under
;;; point. One is "highlight-symbol" used for automatic highlighting after a
;;; delay, and the other is the built-in "hi-lock" used for highlighting using
;;; a key. highlight-symbol's usage of faces is a bit broken which is why we
;;; also use hi-lock.

(require 'init-prefs)

;;; Highlight the line where the cursor is
(when exordium-line-mode
  (global-hl-line-mode 1))


;;; Highlight symbol under point automatically after a small delay.

(eval-when-compile (require 'highlight-symbol))
(when exordium-highlight-symbol
  (require 'highlight-symbol)
  (highlight-symbol-nav-mode)
  ;;
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (setq highlight-symbol-on-navigation-p t)
  ;; Don't show this mode in the modeline
  (eval-after-load 'highlight-symbol
    '(diminish 'highlight-symbol-mode)))


;;; Highlight/unhighlight symbol under point using a key.

(require 'hi-lock)

(defvar exordium-highlighted-symbols ()
  "list of regexps for the currently highlighted symbols. This
  variable is buffer-local.")

(make-variable-buffer-local 'exordium-highlighted-symbols)

(defun exordium-highlight-symbol ()
  "Toggles highlighting of occurrences of the symbol under point
in the current buffer. Up to 4 different symbols can be
highlighted using different colors at one time."
  (interactive)
  (let ((regex (find-tag-default-as-symbol-regexp)))
    (cond ((member regex exordium-highlighted-symbols)
           ;; Remove highlight for this symbol.
           (setq exordium-highlighted-symbols (remove regex exordium-highlighted-symbols))
           (hi-lock-unface-buffer regex))
          (t
           ;; Add highlight for this symbol.
           (setq exordium-highlighted-symbols (cons regex exordium-highlighted-symbols))
           (hi-lock-face-symbol-at-point)))))

(global-set-key (kbd "C-c C-SPC") #'exordium-highlight-symbol)


;;; Highlight color name and Hex values in buffer.
;;;
;;; Mostly convenient for editing Emacs theme files:
;;; run M-x highlight-colors-from-alist and enter an expression like
;;; material-colors or (cdr (assoc 'night tomorrow-colors)).
;;; This will highlight the hex codes and the colors names in buffer.
;;; Note that the expression must evaluate to an a-list like:
;;; ((color1 . "#123456") (color2 . "#345678"))
;;;
;;; Otherwise, there is also the rainbow-mode in Elpa which is more generic but
;;; cannot be set to specific colors.

;; Hexadecimal colors
(defconst exordium-hex-colors-font-lock-keywords
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (1 (exordium-colorize-itself 1))))
  "Font-lock keywords to add for hexadecimal colors.")

(defun highlight-colors-from-list (arg)
  "Highlight any hex color code and any symbol associated with a
color code in ARG, in the current buffer. ARG is an expression
evaluating to an a-list like: ((symbol . hex-string) (symbol . hex-string))"
  (interactive "XExpression: ")
  ;; Highlight hex color codes:
  (font-lock-add-keywords nil exordium-hex-colors-font-lock-keywords t)
  ;; Highlight color symbols defined in arg:
  (let ((arg-font-lock-keywords
         `((,(regexp-opt (mapcar (lambda (pair)
                                   (concat "," (if (stringp (car pair))
                                                   (car pair)
                                                 (symbol-name (car pair)))))
                                 arg))
            (0 (exordium-colorize-by-assoc ',arg))))))
    (font-lock-add-keywords nil `(,@arg-font-lock-keywords) t))
  ;; Force refresh
  (font-lock-mode 1))

(defun exordium-colorize-by-assoc (alist)
  "Colorize a match with its association from ASSOC-LIST."
  (let ((color (substring (match-string-no-properties 0) 1)))
    (exordium-colorize-match (cdr (assoc-string color alist t)))))

(defun exordium-colorize-match (color &optional match)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed according to the
luminance and is either white or black."
  (let ((match (or match 0)))
    (put-text-property
     (match-beginning match) (match-end match)
     'face `((:foreground ,(if (> 0.5 (exordium-x-color-luminance color))
                               "white" "black"))
             (:background ,color)))))

(defun exordium-colorize-itself (&optional match)
  "Colorize a match with itself."
  (exordium-colorize-match (match-string-no-properties (or match 0)) match))

(defun exordium-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun exordium-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (r      (/ (car values) 256.0))
         (g      (/ (cadr values) 256.0))
         (b      (/ (caddr values) 256.0)))
    (exordium-color-luminance r g b)))

(provide 'init-highlight)
