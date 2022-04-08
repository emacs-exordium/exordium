;;;; Small extensions
;;;
;;; Commands:
;;; * M-x `update-config': pulls the latest config from github and recompiles
;;;       all non-melpa Elisp files. You need to restart Emacs afterwards.
;;; * M-x `insert-current-time' at cursor position
;;; * M-x `insert-current-date-time' at cursor position
;;; * M-x `exordium-flip-string-quotes' change quotes between ?\' and ?\"
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
;;; C-c j          Avy: jump to the beginning of any word on the screen. It
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

(use-package cl-lib :ensure nil)
(use-package elisp-refs)


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

(use-package goto-chg)
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

(use-package display-fill-column-indicator
  :if exordium-fci-mode
  :ensure nil
  :demand t
  :bind ("C-|" . display-fill-column-indicator-mode)
  :init
  (defun exordium--select-display-fill-column-indicator-character ()
    (cl-flet
        ((char-or-nil
          (char)
          ;; Return the `char' if displayable. Return nil otherwise.
          ;; This is the same check as in `display-fill-column-indicator-mode'
          ;; but with `string=' the check for faces equality (for some reason
          ;; `eq' doesn't work when initialising).
          (when (and char
                     (char-displayable-p char)
                     (or (not (display-graphic-p))
                         (string=
                          (aref (query-font (car (internal-char-font nil char)))
                                0)
                          (face-font 'default))))
            char)))
      (prog1
          (setq-default display-fill-column-indicator-character
                        (seq-find
                         #'char-or-nil
                         (alist-get (if (eq exordium-fci-use-dashes t)
                                        :one
                                      exordium-fci-use-dashes)
                                    exordium-fci-dashes-alist)))
        (when (and exordium-fci-use-dashes
                   (not display-fill-column-indicator-character))
          (message
           (concat "Selected exordium-fci-dashes: %s with mapped char: %s "
                   "cannot be used as a display-fill-column-indicator-character "
                   " with the face-font: %s.")
           exordium-fci-use-dashes
           (alist-get exordium-fci-use-dashes exordium-fci-dashes-alist)
           (face-font 'default))))))
  (exordium--select-display-fill-column-indicator-character)

  :config
  (cond
   ((eq exordium-fci-mode :always)
    (global-display-fill-column-indicator-mode))
   ((eq exordium-fci-mode :prog)
    (add-hook 'prog-mode-hook
              #'display-fill-column-indicator-mode)))
  ;; `init-util' is loaded only after `init-look-and-feel', so let's do advice
  (define-advice exordium-set-font (:after-while (&rest _args))
    (exordium--select-display-fill-column-indicator-character)))


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


;;; Flip quotes in a string

(defun exordium-flip-string--even-chars-between (char pos-start pos-end)
  "Return t when there is an even number of CHAR between POS-START and POS-END.
The check is starting from `char-before' the POS-END and
continues until at most POS-START."
  (let ((offset 0)
        even)
    (while (and (< pos-start (- pos-end offset 1))
                (eq (char-before (- pos-end offset)) char)
                (eq (char-before (- pos-end offset 1)) char))
      (setq even (if (< pos-start (- pos-end offset 2))
                     (not (eq (char-before (- pos-end offset 2)) char))
                   t))
      (cl-incf offset 2))
    even))

(defun exordium-flip-string-quotes (&optional flip-inner)
  "Flip quotes in a string.

Unless a region is active use syntax in the current buffer to
determine the string at point.  When a region is active ignore syntax
in current buffer and assume the active region is the string.

With a FLIP-INNER prefix, also flip all quotes in the string.
Otherwise escape quotes in the inner string (rationalising escaping)."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      ;; Starting with emacs-28 python strings are split when using a generic
      ;; string delimeter: first and last two quotes are a separate sexp, and
      ;; the `forward-sexp' only works from the most inner quote.  However,
      ;; in emacs-27, the `forward-sexp' works only from the most outer quote.
      (when-let ((orig-start (if (region-active-p)
                                 (let* ((pt (min (region-beginning)
                                                 (region-end)))
                                        (c (char-after pt)))
                                   (when (or (eq c ?\') (eq c ?\"))
                                     pt))
                               (when-let ((pos (nth 8 (syntax-ppss)))
                                          (c (char-after pos)))
                                 (if (and (< (1+ (point-min)) pos)
                                          (eq c (char-after (- pos 1)))
                                          (eq c (char-after (- pos 2))))
                                     (- pos 2)
                                   (- pos 0)))))
                 (orig-quote (char-after orig-start))
                 (new-quote (pcase orig-quote
                              (?\' ?\")
                              (?\" ?\')))
                 (quote-length (if (and (eq orig-quote
                                             (char-after (+ orig-start 1)))
                                        (eq orig-quote
                                            (char-after (+ orig-start 2))))
                                   3
                                 1))
                 (orig-end (if (region-active-p)
                               (let* ((pt (max (region-beginning)
                                               (region-end)))
                                      (c (char-after (1- pt))))
                                 (when  (eq c orig-quote)
                                   pt))
                             (save-excursion
                               (goto-char orig-start)
                               (forward-char (if (version< emacs-version "28")
                                                 0
                                               (logand quote-length 2)))
                               (forward-sexp)
                               (forward-char (if (version< emacs-version "28")
                                                 0
                                               (logand quote-length 2)))
                               (point)))))
        (goto-char orig-start)
        (delete-char quote-length)
        (insert-char new-quote quote-length)
        (while (< (point) (- orig-end quote-length))
          (if flip-inner
              (if-let ((a-quote (pcase (char-after)
                                  (?\' ?\")
                                  (?\" ?\'))))
                  (progn
                    (delete-char 1)
                    (insert-char a-quote))
                (forward-char))
            (cond
             ((eq (char-after) new-quote)
              (if (eq quote-length 1)
                  (when (or (not (eq (char-before) ?\\))
                            (exordium-flip-string--even-chars-between
                             ?\\ orig-start (point)))
                    (insert-char ?\\)
                    (cl-incf orig-end))
                (when (and (< (point) (- orig-end 2))
                           (eq (char-after (+ 1 (point))) new-quote)
                           (eq (char-after (+ 2 (point))) new-quote))
                  ;; assume generic string delimeter has a length of 3
                  (insert-char ?\\)
                  (forward-char)
                  (insert-char ?\\)
                  (forward-char)
                  (insert-char ?\\)
                  (cl-incf orig-end 3))))
             ((and
               (eq (char-after) orig-quote)
               (eq (char-before) ?\\))
              (backward-char)
              (delete-char 1)
              (cl-decf orig-end)))
            (forward-char)))
        ;; A special case: the if the last quote in a string with a generic
        ;; string delimiter `(eq quote-length 3)' is the same as the new-quote
        ;; it needs to be escaped (or the flipped string will end up
        ;; prematurely).
        (when (and (not flip-inner)
                   (eq (char-before) new-quote)
                   (eq quote-length 3)
                   (< 5 (- orig-end orig-start))
                   (or
                    (not (eq (char-before (- (point) 1)) ?\\))
                    (exordium-flip-string--even-chars-between
                     ?\\ orig-start (- (point) 1))))
          (backward-char)
          (insert-char ?\\)
          (forward-char))
        (delete-char quote-length)
        (insert-char new-quote quote-length)))))


;; Find obsolete cl aliases

(defun exordium-elisp-refs--show-results-maybe (orig-fun &rest args)
  "Only show a *refs* buffer when there are results.

This is meant to be applied as an advice around `elisp-refs--show-results'."
  (when (cl-third args)
    (apply orig-fun args)))

(defvar exordium-refs-cl-aliases--advice
  #'exordium-elisp-refs--show-results-maybe
  "An advice to be used around `elisp-refs-show-results'.")

(defun exordium-refs-cl-aliases (&optional show-all)
  "Find all exordium references to aliases defined in an obsolete `cl.el'.

It may produce multiple *refs* buffers, one for each alias uses.

With SHOW-ALL prefix show references in the whole `.emacs.d'
directory.  Otherwise show only references in `.emacs.d/modules'
subdirectory.  Note that scanning the whole `.emacs.d' also
includes scanning `.emacs.d/elpa' (and similar) so it may take
considerably longer time and yield numerous superfluous results
from installed packages.

The list of aliases comes from
https://github.com/emacs-mirror/emacs/blob/9b4a2dd/lisp/obsolete/cl.el#L118-L291
and may require updates."
  (interactive "P")
  (dolist (fun '(
                 (get* . cl-get)
                 (random* . cl-random)
                 (rem* . cl-rem)
                 (mod* . cl-mod)
                 (round* . cl-round)
                 (truncate* . cl-truncate)
                 (ceiling* . cl-ceiling)
                 (floor* . cl-floor)
                 (rassoc* . cl-rassoc)
                 (assoc* . cl-assoc)
                 (member* . cl-member)
                 (delete* . cl-delete)
                 (remove* . cl-remove)
                 (defsubst* . cl-defsubst)
                 (sort* . cl-sort)
                 (function* . cl-function)
                 (defmacro* . cl-defmacro)
                 (defun* . cl-defun)
                 (mapcar* . cl-mapcar)
                 remprop
                 getf
                 tailp
                 list-length
                 nreconc
                 revappend
                 concatenate
                 subseq
                 random-state-p
                 make-random-state
                 signum
                 isqrt
                 lcm
                 gcd
                 notevery
                 notany
                 every
                 some
                 mapcon
                 mapl
                 maplist
                 map
                 equalp
                 coerce
                 tree-equal
                 nsublis
                 sublis
                 nsubst-if-not
                 nsubst-if
                 nsubst
                 subst-if-not
                 subst-if
                 subsetp
                 nset-exclusive-or
                 set-exclusive-or
                 nset-difference
                 set-difference
                 nintersection
                 intersection
                 nunion
                 union
                 rassoc-if-not
                 rassoc-if
                 assoc-if-not
                 assoc-if
                 member-if-not
                 member-if
                 merge
                 stable-sort
                 search
                 mismatch
                 count-if-not
                 count-if
                 count
                 position-if-not
                 position-if
                 position
                 find-if-not
                 find-if
                 find
                 nsubstitute-if-not
                 nsubstitute-if
                 nsubstitute
                 substitute-if-not
                 substitute-if
                 substitute
                 delete-duplicates
                 remove-duplicates
                 delete-if-not
                 delete-if
                 remove-if-not
                 remove-if
                 replace
                 fill
                 reduce
                 compiler-macroexpand
                 define-compiler-macro
                 assert
                 check-type
                 typep
                 deftype
                 defstruct
                 callf2
                 callf
                 letf*
                 letf
                 rotatef
                 shiftf
                 remf
                 psetf
                 (define-setf-method . define-setf-expander)
                 the
                 locally
                 multiple-value-setq
                 multiple-value-bind
                 symbol-macrolet
                 macrolet
                 progv
                 psetq
                 do-all-symbols
                 do-symbols
                 do*
                 do
                 loop
                 return-from
                 return
                 block
                 etypecase
                 typecase
                 ecase
                 case
                 load-time-value
                 eval-when
                 destructuring-bind
                 gentemp
                 pairlis
                 acons
                 subst
                 adjoin
                 copy-list
                 ldiff
                 list*
                 tenth
                 ninth
                 eighth
                 seventh
                 sixth
                 fifth
                 fourth
                 third
                 endp
                 rest
                 second
                 first
                 svref
                 copy-seq
                 evenp
                 oddp
                 minusp
                 plusp
                 floatp-safe
                 declaim
                 proclaim
                 nth-value
                 multiple-value-call
                 multiple-value-apply
                 multiple-value-list
                 values-list
                 values
                 pushnew
                 decf
                 incf
                 ))
    (advice-add 'elisp-refs--show-results :around exordium-refs-cl-aliases--advice)
    (let ((path (concat (file-name-directory (expand-file-name user-emacs-directory))
                        (unless show-all "modules")))
          (new (if (consp fun) (prog1 (cdr fun) (setq fun (car fun)))
                 (intern (format "cl-%s" fun)))))
      (cond
       ((functionp fun)
        (elisp-refs-function fun path))
       ((macrop fun)
        (elisp-refs-macro fun path))))
    (advice-remove 'elisp-refs--show-results exordium-refs-cl-aliases--advice)))

(provide 'init-util)
