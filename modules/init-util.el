;;;; Small extensions
;;;
;;; Commands:
;;; * M-x `update-config': pulls the latest config from github and recompiles
;;;       all non-melpa Elisp files. You need to restart Emacs afterwards.
;;; * M-x `insert-current-time' at cursor position
;;; * M-x `insert-current-date-time' at cursor position
;;;
;;; Keys:
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-%            `goto-matching-paren'
;;;
;;; C-c s          Push point onto position stack ("s = save")
;;; C-c b          Pop point from position stack ("b = back")
;;;
;;; C-x C-\        Goto to last change (then second most recent edit, etc.)
;;; C-x C-|        Add shift to reverse the direction
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
;;; Ctrl-|         Toggle FCI mode on and off ("Fill columm indicator",
;;;                e.g. the 80 column ruler)


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


;;; Bookmark position stack

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

(global-set-key [(control c)(s)] 'postack-push)
(global-set-key [(control c)(b)] 'postack-pop)


;;; Goto last change

(require 'goto-chg)
(define-key global-map [(control x)(control \\)] 'goto-last-change)
(define-key global-map [(control x)(control |)] 'goto-last-change-reverse)


;;; Insert date/time

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


;;; Copy the whole buffer without changing the current position

(defun copy-all ()
  "Copy the entire buffer to the clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))


;;; Duplicate lines

(defun duplicate-line-or-region (arg)
  "Duplicate current line or region, leaving point in lower line."
  (interactive "*p")
  ;; Save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (if mark-active (region-beginning)
               (save-excursion (beginning-of-line) (point))))
        eol
        (num-lines (if mark-active
                       (count-lines (region-beginning) (region-end))
                     1)))
    (save-excursion
      (if mark-active
          (setq eol (region-end))
        (end-of-line)
        (setq eol (point)))
      ;; Disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t))
        ;; Insert the line arg times
        (dotimes (i (if (> arg 0) arg 1))
          (unless (pg/string-ends-with line "\n")
            (newline))
          (insert line)))
      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ;; Move the point to the lowest line
    (forward-line (* arg num-lines))))

(global-set-key [(control c)(d)] 'duplicate-line-or-region)


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

(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len  (or len fill-column))
  (let ((start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (called-interactively-p)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      (goto-line start-line)
      (message "Not found"))))


;;; Highlight symbol

(when *init-highlight-symbol*
  (require 'highlight-symbol)
  (highlight-symbol-nav-mode)
  ;;
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (setq highlight-symbol-on-navigation-p t)
  ;; Don't show this mode in the modeline
  (eval-after-load 'highlight-symbol
    '(diminish 'highlight-symbol-mode)))


;;; Buffers and windows

(defun toggle-window-dedicated ()
  "Toggles whether the current active window is dedicated or not"
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (message (if (set-window-dedicated-p window
                                         (not (window-dedicated-p window)))
                 "Window '%s' is dedicated"
               "Window '%s' is normal")
             (current-buffer))))

;;; Note: apparently there is no Pause key on an Apple keyboard...
(define-key global-map [pause] 'toggle-window-dedicated)

(defun kill-all-buffers ()
  "Kill all buffers that are associated with a file."
  (interactive)
  (mapc #'(lambda (buff)
            (when (buffer-file-name buff)
              (kill-buffer buff)))
        (buffer-list)))


;;; Config management

(defun update-config ()
  "Updates the configuration. Specifically, pulls from github and
compiles all non-melpa elisp files. You need to restart Emacs
afterwards."
  (interactive)
  (cd "~/.emacs.d")
  (shell-command "git pull")
  (byte-recompile-directory "~/.emacs.d/modules" 0)
  (byte-recompile-directory "~/.emacs.d/themes" 0)
  (byte-recompile-directory "~/.emacs.d/extensions" 0)
  (message (propertize "Restart Emacs to make any changes effective"
                       'face 'error)))

(defun uncompile-modules ()
  "Uncompiles all modules and themes. This is handy for development"
    (interactive)
  (dolist (dir '("~/.emacs.d/modules" "~/.emacs.d/themes" "~/.emacs.d/extensions"))
    (when (file-directory-p dir)
      (dolist (elc (directory-files dir t "\\.elc$"))
        (warn "Removing .elc file: %s" elc)
        (delete-file elc)))))

(defun force-recompile-modules ()
  "Recompile all modules and themes"
  (interactive)
  (dolist (dir '("~/.emacs.d/modules" "~/.emacs.d/themes" "~/.emacs.d/extensions"))
    (when (file-directory-p dir)
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (file-exists-p elc)
            (delete-file elc))
          (byte-compile-file el))))))


(provide 'init-util)
