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
;;; C-|            Toggle FCI mode on and off ("Fill columm indicator",
;;;                e.g. the 80 column ruler)
;;;
;;; C-j            Avy: jump to the beginning of any word on the screen. It
;;; C-'            asks for the first character of the word you want, and then
;;;                annotates each word that starts with this character with a
;;;                unique code of 1 or 2 letters. Type this code to jump
;;;                directly to the word. Note that the codes it generates are
;;;                optimized for touch-type.
;;;
;;; M-Q            `unfill-paragraph': the opposite of M-q.
;;;
;;; Functions:
;;;
;;; `kill-all-buffers' does what you expect.
;;;
;;; `new-scratch' creates a new scratch buffer with a unique name, in
;;; fundamental mode. This buffer doesn't need to be saved before being killed.

(require 'init-lib)
(require 'init-prefs)


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

(defun postack-goto (pos)
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
(define-key global-map [(control x)(control /)] 'goto-last-change-reverse)


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
                     1))
        (col (current-column)))
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
          (unless (string-suffix-p "\n" line)
            (newline))
          (insert line)))
      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ;; Move the point to the lowest line
    (forward-line (* arg num-lines))
    (when (= num-lines 1)
      ;; Leave the cursor an the same column if we duplicated 1 line
      (move-to-column col))))

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


;;; Moving between words
;;;
;;; By default C-Right = M-Right = `right-word',
;;; and        C-Left  = M-left  = `left-word'.
;;; These functions move the cursor to the beginning or the end of words.
;;; Unfortunately they are not symetrical: moving right then left does not
;;; necessarily bring you back where you started. Here we redefine M-left
;;; and M-right to move following the syntax: the motion is slower but
;;; symetrical. C-Left and C-Right are left unchanged (move by words).

(define-key global-map [(meta right)]
  #'(lambda (arg)
    (interactive "p")
    (forward-same-syntax arg)))

(define-key global-map [(meta left)]
  #'(lambda (arg)
      (interactive "p")
      (forward-same-syntax (- arg))))


;;; FCI: 80-column ruler bound to Ctrl-|
;;; Note: if it causes Emacs to crash on images, set the variable
;;; fci-always-use-textual-rule to t (it will use a character instead).

(eval-when-compile
  (require 'fill-column-indicator))

(when exordium-fci-mode
  (require 'fill-column-indicator)

  (when exordium-fci-use-dashes
    (setq fci-rule-use-dashes t)
    (setq fci-dash-pattern 0.5))
  (setq fci-rule-width 1)

  (define-key global-map [(control |)]
    #'(lambda ()
        (interactive)
        (fci-mode (if fci-mode 0 1))))

  (when (eq exordium-fci-mode :always)
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (global-fci-mode 1)))

;;; Fix a display bug in auto-complete caused by FCI. See
;;; https://github.com/alpaker/Fill-Column-Indicator/issues/21

(when (and exordium-fci-mode
           (eq exordium-complete-mode :auto-complete)
           exordium-fci-fix-autocomplete-bug)
  (require 'fill-column-indicator)
  (require 'popup)

  ;;; fci-mode turns line truncation on by default (see
  ;;; `fci-handle-truncate-lines'); this is pretty annoying when you have long
  ;;; lines and fci-mode goes off (to display popup) and your line truncation
  ;;; goes off. Hence, for popups we turn off this handler and let `fc-mode'
  ;;; relay on whatever buffer value of `truncate-lines' is.

  (defvar exordium-fci-mode-suppressed nil)

  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (and (boundp 'fci-mode) fci-mode)))
      (when fci-enabled
        (set (make-local-variable 'exordium-fci-mode-suppressed) fci-enabled)
        (set (make-local-variable 'exordium-fci-handle-truncate-lines)
             fci-handle-truncate-lines)
        (setq fci-handle-truncate-lines nil)
        (turn-off-fci-mode))))

  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and exordium-fci-mode-suppressed
               (null popup-instances))
      (setq exordium-fci-mode-suppressed nil)
      (setq fci-handle-truncate-lines 'exordium-fci-handle-truncate-lines)
      (turn-on-fci-mode))))


;;; Avy - go to any word on the screen in just 2 or 3 keystrokes.
;;; C-c j => asks for a character, then one or 2 keys to jump.
;;; Note: Avy has other commands, this is the most useful.

(global-set-key [(control c)(j)] #'avy-goto-word-or-subword-1)
(global-set-key [(control ?\')] 'avy-goto-word-or-subword-1)


;;; Finding lines that are too long (according to some code styles).

(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len (or len fill-column))
  (let ((start-line (line-number-at-pos))
        (len-found  0)
        (found      nil))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (called-interactively-p 'interactive)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      ;; Compiler-happy equivalent to (goto-line start-line):
      (goto-char (point-min))
      (forward-line (1- start-line))
      (message "Not found"))))


;;; Buffers

(defun kill-all-buffers ()
  "Kill all buffers that are associated with a file."
  (interactive)
  (mapc #'(lambda (buff)
            (when (buffer-file-name buff)
              (kill-buffer buff)))
        (buffer-list)))

(defun scratch ()
  "Create a new scratch buffer that does not need to be
saved. This is useful for editing snippets of text in a temporary
buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))


;;; Miscellaneous

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)


;;; Config management

(defun update-taps ()
  "Updates each installed tap. Specifically, for each tap it pulls from github."
  (when (file-accessible-directory-p exordium-taps-root)
    (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
      (when (file-accessible-directory-p tap)
        (cd tap)
        (shell-command "git pull")))))

(defun update-config ()
  "Updates the configuration. Specifically, pulls from github and
compiles all non-melpa elisp files. You need to restart Emacs
afterwards."
  (interactive)
  (cd user-emacs-directory)
  (shell-command "git pull")
  (byte-recompile-directory exordium-modules-dir 0)
  (byte-recompile-directory exordium-themes-dir 0)
  (byte-recompile-directory exordium-extensions-dir 0)
  (unless exordium-skip-taps-update
    (update-taps))
  (message (propertize "Restart Emacs to make any changes effective"
                       'face 'error)))

(defun uncompile-modules ()
  "Uncompiles all modules and themes. This is handy for development"
    (interactive)
    (dolist (dir (list exordium-modules-dir
                       exordium-themes-dir
                       exordium-extensions-dir
                       exordium-local-dir))
    (when (file-directory-p dir)
      (dolist (elc (directory-files dir t "\\.elc$"))
        (warn "Removing .elc file: %s" elc)
        (delete-file elc)))))

(defun force-recompile-modules ()
  "Recompile all modules and themes"
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      (dolist (el (directory-files dir t "\\.el$"))
        (unless (string-suffix-p ".t.el" el)
          (let ((elc (byte-compile-dest-file el)))
            (when (file-exists-p elc)
              (delete-file elc))
            (byte-compile-file el)))))))

(defun link-local-config (local-dir)
  (interactive "Dlocal directory to symlink files from")
  (make-symbolic-link (concat local-dir "prefs.el")
                      (locate-user-emacs-file "prefs.el")
                      't)
  (make-symbolic-link (concat local-dir "after-init.el")
                      (locate-user-emacs-file "after-init.el")
                      't)
  (make-symbolic-link (concat local-dir "before-init.el")
                      (locate-user-emacs-file "before-init.el")
                      't))

(defun config-status ()
  "Check if the configuration is up to date and display a
  message"
  (interactive)
  (cl-flet ((sh (cmd)
                ;; Execute cmd in dir and return output
              (message "Running: %s" cmd)
              (shell-command-to-string (concat "cd " user-emacs-directory " && " cmd))))
    (if (> (length (sh "git diff --shortstat")) 0)
        (message (propertize "Exordium repo is not clean" 'face 'error))
      (let ((st (progn
                  (sh "git remote update")
                  (sh "git status -uno"))))
        (cond ((string-match ".+\n.+ behind 'origin/master' by \\([0-9]+\\) commit" st)
               (message (propertize
                         (format "Exordium is %s commit(s) behind" (match-string 1 st))
                         'face 'error)))
               ((string-match ".+\nYour branch is up-to-date" st)
                (message (propertize "Exordium is up-to-date"
                                     'face 'success)))
               (t
                (message "Can't tell (are you on the master branch?)")))))))



(provide 'init-util)
