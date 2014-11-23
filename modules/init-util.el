;;;; Small extensions
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-%            `goto-matching-paren'
;;;
;;; C-c C-s        Push point onto position stack ("s = save")
;;; C-c C-b        Pop point from position stack ("b = back")
;;;
;;; (unbound)      `insert-current-time'
;;; (unbound)      `insert-current-date-time'
;;;
;;; C-c d          `duplicate-line'
;;;
;;; C-\            `delete-horizontal-space-forward'
;;; C-BACKSPACE    `delete-horizontal-space-backward'
;;;                (Emacs also has Meta-\ to delete all spaces)
;;;
;;; M-d            `delete-word'
;;; M-BACKSPACE    `backward-delete-word'
;;;
;;; Ctrl-=         Expand region
;;; Ctrl-|         Toggle fci mode on and off (80 column ruler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Match parentheses
;;; Ctrl-% = go to match paren

(defun goto-match-paren-or-up (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to
   the opening parenthesis one level up."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1))
           (t
            (backward-char 1)
            (cond ((looking-at "\\s\)")
                   (forward-char 1) (backward-list 1))
                  (t
                   (while (not (looking-at "\\s("))
                     (backward-char 1)
                     (cond ((looking-at "\\s\)")
                            (message "->> )")
                            (forward-char 1)
                            (backward-list 1)
                            (backward-char 1)))))))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise
   insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t                    (self-insert-command (or arg 1)))))

(global-set-key [(control %)] 'goto-match-paren-or-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Position stack
;;; Ctrl-C M = push position
;;; Ctrl-C P = pop (go back to previous position)

(defvar postack-stack '() "The position stack")

(defun postack-goto (marker)
  "Should be marker-goto."
  (switch-to-buffer (marker-buffer pos))
  (goto-char (marker-position pos)))

(defun postack-push ()
  "Push the current position on the position stack."
  (interactive)
  (let ((pos (point-marker)))
    (setq postack-stack (cons pos postack-stack))
    (message (format "Marked: (%s:%s)"
                    (marker-buffer pos)
                    (marker-position pos))) ))

(defun postack-pop ()
  "Remove the top position from the position stack and make it current."
  (interactive)
  (let ((pos (car postack-stack)))
    (setq postack-stack (cdr postack-stack))
    (cond ((null pos)
           (message "Position stack empty"))
          ((markerp pos)
           (postack-goto pos)
           (message (format "Position: (%s:%s)"
                            (marker-buffer pos)
                            (marker-position pos))))
          (t
           (message "Invalid position in stack")))))

(global-set-key [(control c)(control s)] 'postack-push)
(global-set-key [(control c)(control b)] 'postack-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert date/time
;;; Ctrl-C Ctrl-D = date
;;; Ctrl-C Ctrl-T = time

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))

(defun insert-current-time ()
  "insert the current time into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Duplicate lines

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key [(control c)(d)] 'duplicate-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting Spaces

(defun delete-horizontal-space-forward ()
  "Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(global-set-key [(control \\)] 'delete-horizontal-space-forward)

(defun delete-horizontal-space-backward ()
  "Delete all spaces and tabs before point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point))))

(global-set-key [(control backspace)] 'delete-horizontal-space-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting Words
;;;
;;; These functions replace delete-word and backward-kill-word: they do NOT put
;;; the deleted word onto the clipboard. Otherwise this gets annoying:
;;; - copy some expression
;;; - Delete an argument in a funcall using M-d or M-backspace
;;; - try to yank the the expression to replace the argument...

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(define-key global-map [(meta d)] 'delete-word)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(define-key global-map [(meta backspace)] 'backward-delete-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autopairs

(when *init-enable-electric-pair-mode*
  (electric-pair-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 80-column ruler bound to Ctrl-|
;;; Note: if it causes Emacs to crash on images, set the variable
;;; fci-always-use-textual-rule to t (it will use a character instead).

(when *init-fci-mode*
  (require 'fill-column-indicator)
  (when *init-fci-use-dashes*
    (setq fci-rule-use-dashes t)
    (setq fci-dash-pattern 0.5))
  (setq fci-rule-width 1)
  (setq fci-rule-color "dim gray")
  (define-key global-map [(control |)] 'fci-mode)
  (when (eq *init-fci-mode* :always)
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (global-fci-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight symbol

(require 'highlight-symbol)

(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; Don't show this mode in the modeline
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(provide 'init-util)
