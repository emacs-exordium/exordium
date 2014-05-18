
;;; Formatting function arguments (WORK IN PROGRESS) --------------------------

;;; TODO use this for argument formatting:
(defun bde-goto-match-paren (arg)
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
                             (backward-char 1)))
                      ))))))
 (global-set-key [(control %)] 'bde-goto-match-paren)

(defun bde-align-arguments (arg)
  "Align function arguments according to the BDE style. Cursor
must be inside the argument list."
  (interactive "p")
  (bde-goto-match-paren (point))
  (push-mark)
  (bde-goto-match-paren (point))
  (kill-region (region-beginning) (region-end))
  (with-temp-buffer
    (yank)
    ;;(mark-whole-buffer)
    (beginning-of-buffer)
    (forward-char 1)
    (push-mark)
    (end-of-buffer)
    (backward-char 1)
    (kill-region (mark) (point))
    ;; (let ((s (region-substring (region-beginning) (region-end))))
    ;;   (message "Yoo")))
    )
  (yank)
  )

(global-set-key [(control c)(a)] 'bde-align-arguments)

(defun center-rectangle (beg end)
  (interactive "*r")
  (kill-rectangle beg end)
  (with-temp-buffer
    (yank-rectangle)
    (setq fill-column (current-column))
    (center-region (point-min) (point-max))
    (goto-char (point-max))
    (move-to-column fill-column t)
    (kill-rectangle (point-min) (point)))
  (goto-char beg)
  (yank-rectangle))
