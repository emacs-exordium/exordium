;;; init-highlight.el --- Highlighting things -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module controls the highlighting of the current line and of the symbol
;; under point.
;;
;; If `exordium-line-mode' is t, the line under the cursor is highlighted.
;;
;; If `exordium-highlight-symbol-at-point' is t, the symbol under the cursor is
;; highlighted after a small delay using a dim background.
;;
;; Alternatively, one can use the key C-c C-SPACE to highlight/un-highlight the
;; symbol under the cursor in the current buffer.  Up to 4 different symbols
;; may be highlighted at one time, using different colors.  Feel free to rebind
;; function `exordium-highlight-symbol-at-point' to a better key, like for example:
;;
;; (bind-key "C-RET" #'exordium-highlight-symbol-at-point)
;; (bind-key "<f6>" #'exordium-highlight-symbol-at-point)
;;
;; Notes: Exordium uses 2 packages for highlighting the symbol under
;; point.  One is "highlight-symbol" used for automatic highlighting after a
;; delay, and the other is the built-in "hi-lock" used for highlighting using
;; a key.  highlight-symbol's usage of faces is a bit broken which is why we
;; also use hi-lock.

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package highlight
  :defer t)

;;; Highlight the line where the cursor is
(use-package hl-line
  :ensure nil
  :when exordium-line-mode
  :config
  (global-hl-line-mode +1))


;;; Highlight symbol under point automatically after a small delay.
(when exordium-highlight-symbol
  (use-package highlight-symbol
    ;; N.B. the `highlight-symbol' package has not been updated for a while (at
    ;; the time of writing this comment in 2024 last update was from 2016) and
    ;; it forcibly defines its own alias for `highlight-symbol-at-point',
    ;; clobbering the one delivered with Emacs in `hi-lock'. However, the
    ;; `highlight-symbol' implementation is nicer, as it allows to remove
    ;; highlighting with the same keybinding. Also the `highlight-symbol-mode'
    ;; is the mode that is turned on (`hi-lock-mode' is not turned on), so
    ;; let's keep that implementation, by ensuring load order.
    :after (hi-lock)
    ;; Also, the `highlight-symbol-flush' implementation is not accounting for
    ;; `jit-lock-mode', which causes a few seconds delay when automatically
    ;; highlighted symbols.  Call `font-lock-ensure' to speed things up, but
    ;; limit it to the visible portion of a buffer.
    :functions (exordium--highlight-symbol-ensure)
    :init
    (defun exordium--highlight-symbol-ensure ()
      (when (eq (font-lock-value-in-major-mode font-lock-support-mode)
              'jit-lock-mode)
        (font-lock-ensure (window-start) (window-end))))

    :diminish highlight-symbol-mode
    :hook ((prog-mode . highlight-symbol-mode)
           (prog-mode . highlight-symbol-nav-mode))
    :custom
    (highlight-symbol-on-navigation-p t)
    :config
    (advice-add 'highlight-symbol-flush
                :after #'exordium--highlight-symbol-ensure)))


;;; Highlight/unhighlight symbol under point using a key.

(defvar-local exordium-highlighted-symbols ()
  "List of regexps for the currently highlighted symbols.
This variable is buffer-local.")

(use-package hi-lock
  :ensure nil
  :demand t
  :autoload (hi-lock-regexp-okay)
  :functions (exordium-highlight-symbol-at-point)
  :init
  (defun exordium-highlight-symbol-at-point ()
    "Toggle highlighting of occurrences of the symbol under point.
Faces from `hi-lock-face-defaults' are used to perform the highlight, so up
to the number of elements in that list of different symbols can
be highlighted using different colors at one time."
    (interactive)
    (when-let* ((regexp (ignore-errors (hi-lock-regexp-okay
                                        (find-tag-default-as-symbol-regexp)))))
      (cond ((member regexp exordium-highlighted-symbols)
             ;; Remove highlight for this symbol.
             (setq exordium-highlighted-symbols (remove regexp exordium-highlighted-symbols))
             (hi-lock-unface-buffer regexp))
            (t
             ;; Add highlight for this symbol.
             (setq exordium-highlighted-symbols (cons regexp exordium-highlighted-symbols))
             (hi-lock-face-symbol-at-point)))))
  :bind  ("C-c C-SPC" . #'exordium-highlight-symbol-at-point))


;; Highlight color name and Hex values in buffer.
;;
;; Mostly convenient for editing Emacs theme files:
;; run M-x highlight-colors-from-alist and enter an expression like
;; material-colors or (cdr (assoc 'night tomorrow-colors)).
;; This will highlight the hex codes and the colors names in buffer.
;; Note that the expression must evaluate to an a-list like:
;; ((color1 . "#123456") (color2 . "#345678"))
;;
;; Otherwise, there is also the rainbow-mode in Elpa which is more generic but
;; cannot be set to specific colors.

;; Hexadecimal colors
(defconst exordium-hex-colors-font-lock-keywords
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)"
     (1 (exordium-colorize-itself 1))))
  "Font-lock keywords to add for hexadecimal colors.")

(defun highlight-colors-from-list (arg)
  "Highlight any hex color code and any symbol associated with ARG.
ARG is an expression evaluating to an a-list like:
\((symbol . hex-string) (symbol . hex-string))"
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
  "Colorize a match with its association from ALIST."
  (let ((color (substring (match-string-no-properties 0) 1)))
    (exordium-colorize-match (cdr (assoc-string color alist t)))))

(defun exordium-colorize-match (color &optional match)
  "Return matched string propertized with face that background is COLOR.
The foreground is computed according to the luminance and is
either white or black.  Optional MATCH denotes current match in
search data and defaults to 0."
  (let ((match (or match 0)))
    (put-text-property
     (match-beginning match) (match-end match)
     'face `((:foreground ,(if (> 0.5 (exordium-x-color-luminance color))
                               "white" "black"))
             (:background ,color)))))

(defun exordium-colorize-itself (&optional match)
  "Colorize a MATCH with itself."
  (exordium-colorize-match (match-string-no-properties (or match 0)) match))

(defun exordium-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun exordium-x-color-luminance (color)
  "Calculate the luminance of a COLOR string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (color-values color))
         (r      (/ (car values) 256.0))
         (g      (/ (cadr values) 256.0))
         (b      (/ (caddr values) 256.0)))
    (exordium-color-luminance r g b)))

(provide 'init-highlight)

;;; init-highlight.el ends here
