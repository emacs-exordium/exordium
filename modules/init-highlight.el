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

(provide 'init-highlight)
