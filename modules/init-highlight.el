;;; init-highlight.el --- Highlighting things -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module controls the highlighting of the current line and of the symbol
;; under point.
;;
;; If `exordium-line-mode' is t, the line under the cursor is highlighted.
;;
;; If `exordium-highlight-symbol' is t, the symbol under the cursor is
;; highlighted after a small delay using a dim background.
;;
;; Alternatively, one can use the key C-c C-SPC to highlight/un-highlight the
;; symbol under the cursor in the current buffer.  When point is in symbol that
;; has been highlighted it is possible to, for example navigate between
;; different instances of the symbol.  See `symbol-overlay-map-help' and
;; `exordium-highlight-symbol-map-modifier' for more information.

;; Feel free to rebind function `symbol-overlay-put' to a better key, like for
;; example:
;;
;; (bind-key "C-RET" #'sybmol-overlay-put)
;; (bind-key "<f6>" #'sybmol-overlay-put)
;;

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

;;; Highlight the line where the cursor is
(use-package hl-line
  :ensure nil
  :when exordium-line-mode
  :config
  (global-hl-line-mode +1))


(use-package symbol-overlay
  :diminish
  :commands (symbol-overlay-map-help)
  :functions (exordium--extract-font-lock-keywords)
  :init
  (when exordium-highlight-symbol
    (let ((form '(add-hook 'prog-mode-hook #'symbol-overlay-mode)))
      (if (boundp 'prog-mode-hook)
          (eval form)
        (eval-after-load 'prog-mode `,form)))
    (when exordium-help-extensions
      (let ((form '(add-hook 'helpful-mode-hook #'symbol-overlay-mode)))
      (if (boundp 'helpful-mode-hook)
          (eval form)
        (eval-after-load 'helpful-mode `,form)))))

  ;; Remove temporary highlighting from Emacs Lisp and Lisp keywords.  N.B.,
  ;; This matches only some of keywords, more basic for example `defun',
  ;; `progn', or `cl-defmacro', but not constructs like `if', `setq' or `let'.
  ;; The latter will still be highlighted.
  (defun exordium--extract-font-lock-keywords (keywords)
    "Extract regexp from `font-lock' style KEYWORDS."
    (with-temp-buffer
      (insert (caar keywords))
      (when-let* ((begin (1+ (point-min)))
                  ((< begin (point-max))))
        (goto-char (1+ (point-min)))
        (re-search-forward (rx "\\_>") nil t)
        (when (< begin (point))
          (concat "\\`" (buffer-substring-no-properties begin (point)))))))

  (require 'lisp-mode)
  (defconst exordium--symbol-overlay-ignore-keywords-el
    (exordium--extract-font-lock-keywords lisp-el-font-lock-keywords-1))
  (defconst exordium--symbol-overlay-ignore-keywords-cl
    (exordium--extract-font-lock-keywords lisp-cl-font-lock-keywords-1))

  (defun exordium--symbol-overlay-ignore-function-el (symbol)
    "Determine whether SYMBOL should be ignored (Emacs-Lisp Language)."
    (when exordium--symbol-overlay-ignore-keywords-el
      (string-match-p exordium--symbol-overlay-ignore-keywords-el symbol)))

  (defun exordium--symbol-overlay-ignore-function-cl (symbol)
    "Determine whether SYMBOL should be ignored (Lisp Language)."
    (when exordium--symbol-overlay-ignore-keywords-cl
      (string-match-p exordium--symbol-overlay-ignore-keywords-cl symbol)))

  :bind
  ("C-c C-SPC" . #'symbol-overlay-put)

  :config
  (unless (get 'symbol-overlay-map
               'exordium-original-value)
    (put 'symbol-overlay-map
         'exordium-original-value
         (copy-keymap symbol-overlay-map)))

  (if-let* ((template (alist-get exordium-highlight-symbol-map-modifier
                                 '((meta . "M-%c")
                                   (control . "C-%c")
                                   (super . "s-%c")
                                   (hyper . "H-%c"))))
              (map (make-sparse-keymap)))
      (progn
        (map-keymap (lambda (key definition)
                      (when (and (functionp definition) (< ?  key 127))
                        (bind-key (format template key) definition map)))
                    (get 'symbol-overlay-map
                         'exordium-original-value))
        (bind-keys :map map
                   ((format template ?N) . symbol-overlay-switch-forward)
                   ((format template ?P) . symbol-overlay-switch-backward))
        (setq symbol-overlay-map map))
    (bind-keys :map symbol-overlay-map
               ("N" . symbol-overlay-switch-forward)
               ("P" . symbol-overlay-switch-backward)))

  (when exordium-help-extensions
    ;; Like in `casual' bind help to "C-o", unless it has been bound above.
    (unless (and (eq exordium-highlight-symbol-map-modifier 'control)
                 (catch 'bound
                   (map-keymap (lambda (key _)
                                 (when (eq key ?o) (throw 'bound key)))
                               (get 'symbol-overlay-map
                                    'exordium-original-value))))
      (bind-keys :map symbol-overlay-map
                 ("C-o" . symbol-overlay-map-help))))

  ;; Add ts-modes handling.  Do it here, as it can't be referred to a definiens
  ;; in a definiendum.  The reason being that, when using :custom, the
  ;; unevaluated definiendum is installed for setting when the relevant
  ;; defcustom is evaluated.  The latter happens when feature is loaded.  This
  ;; is nice as it allows to install a custom standard value for variable,
  ;; without a need to set it to the standard.  However, at such a point
  ;; there's no standard value for the variable (the definiens).
  (dolist (elt (append
                '((emacs-lisp-mode . exordium--symbol-overlay-ignore-function-el)
                  (lisp-interaction-mode . exordium--symbol-overlay-ignore-function-el)
                  (lisp-mode . exordium--symbol-overlay-ignore-function-cl))
                (delq
                 nil (mapcar
                      (lambda (elt)
                        (when-let* ((mode (car elt))
                                    (ts-mode (intern (replace-regexp-in-string
                                                      (rx "-mode" string-end)
                                                      "-ts-mode"
                                                      (symbol-name mode))))
                                    ((not (eq ts-mode mode)))
                                    ((fboundp ts-mode))
                                    ((not (assq ts-mode
                                                symbol-overlay-ignore-functions))))
                          (cons ts-mode (cdr elt))))
                      symbol-overlay-ignore-functions))))
    (unless (assq (car elt) symbol-overlay-ignore-functions)
      (push elt symbol-overlay-ignore-functions))))


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
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)\\{1,4\\}\\)"
     (1 (exordium-colorize-itself 1))))
  "Font-lock keywords to add for hexadecimal colors.")

(defun highlight-colors-from-alist (arg)
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
