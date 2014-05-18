;;;; Small extensions

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

(global-set-key [(control c)(m)] 'postack-push)
(global-set-key [(control c)(p)] 'postack-pop)


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

(global-set-key [(control c)(control d)] 'insert-current-date-time)
(global-set-key [(control c)(control t)] 'insert-current-time)
