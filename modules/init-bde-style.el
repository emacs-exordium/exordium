;;; bde-style.el --- Provide BDE-style C++ indentation
;;;
;;; See https://github.com/bloomberg/bde
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c =          `bde-insert-define-class-header'
;;; C-c -          `bde-insert-declare-class-header'
;;; C->            `bde-aligh-right-after-point': align text after cursor
;;;                to the right (for // RETURN or // LOCK)
;;;
;;; TODO
;;; - Enums are not correct
;;; - Add a function to align function arguments (with * and &)


(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(defun bde-in-member-documentation ()
  "Check if we are looking at a line that is the end of a chain
of comments following a line that ends in a semi-colon,
immediately inside a class or namespace scope."
  (case (caar c-syntactic-context)
    ((inclass innamespace)
     (save-excursion
       (loop
        (beginning-of-line)
        (cond ((= (point) (point-min))
               (return nil))
              ((re-search-forward "^ *//" (point-at-eol) t)
               (next-line -1))
              ((re-search-forward "; *$" (point-at-eol) t)
               (return t))
              (t
               (return nil))))))
    (t nil)))

(defun bde-comment-offset (element)
  "Return a symbol for the correct indentation level at the
current cursor position."
  (if (bde-in-member-documentation)
      '+
    nil))

;;; The offset specifications in c-offset-alist can be any of the following:
;;; - An integer -> specifies a relative offset. All relative offsets will be
;;;   added together and used to calculate the indentation relative to an
;;;   anchor position earlier in the buffer.
;;; - One of the symbols +, -, ++, --, *, or /
;;;   +   c-basic-offset times 1
;;;   -   c-basic-offset times −1
;;;   ++  c-basic-offset times 2
;;;   --  c-basic-offset times −2
;;;   *   c-basic-offset times 0.5
;;;   /   c-basic-offset times −0.5

(c-add-style
 "bde"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset . 0)
   (c-offsets-alist
    (comment-intro         . bde-comment-offset)
    (defun-open            . 0)
    (defun-close           . 0)
    (statement-block-intro . +)
    (substatement-open     . 0)
    (substatement-label    . 0)
    (label                 . 0)
    (access-label          . /)
    (statement-cont        . +)
    (inline-open           . 0)
    (inline-close          . 0)
    (innamespace           . 0)
    (member-init-intro     . 0)
    (extern-lang-open      . 0)
    (brace-list-entry      . /)
    (extern-lang-close     . 0))))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode  . "awk")
        (c++-mode  . "bde")
        (other     . "gnu")))

;;; Enable auto indent
(setq-default c-tab-always-indent t)

;;; Use M-i to go to the next 4-character tab position
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Allow tab in Makefile
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert class header

;;; Note: left-char and right-char pnly exist in emacs 24, so we use
;;; backward-char and forward-char instead.
(defun bde-insert-class-header (header-char)
  (let ((erase-hint t))
    (flet ((delete-header-char (n)
             (save-excursion
               (previous-line 1)
               (end-of-line)
               (backward-delete-char n)
               (next-line 2)
               (end-of-line)
               (backward-delete-char n)))
           (center-header ()
             (save-excursion
               (previous-line 1)
               (center-line 3))))
      ;; Get started
      (dotimes (i 3)
        (insert "// ")
        (newline))
      (previous-line 2)
      (end-of-line)
      (center-header)
      (insert " <class name>")
      (backward-char 12)
      ;; Read and process input
      (loop
       (let ((c (read-event "Inserting header")))
         (cond ((integerp c)
                (when erase-hint (kill-line))
                (insert-char c 1)
                (save-excursion
                  (previous-line 1)
                  (when erase-hint
                    (end-of-line)
                    (insert " "))
                  (insert-char header-char 1)
                  (next-line 2)
                  (when erase-hint
                    (end-of-line)
                    (insert " "))
                  (insert-char header-char 1))
                (unless (= c 32)
                  (center-header)))
               ((eq c 'backspace)
                (backward-delete-char 1)
                (delete-header-char 1))
               ((eq c 'kp-delete)
                (delete-char 1)
                (delete-header-char 1))
               ((eq c 'left)
                (backward-char 1))
               ((eq c 'right)
                (forward-char 1))
               (t
                (return nil)))
         (setq erase-hint nil))))))

(defun bde-insert-define-class-header ()
  "Mini-mode for creating a BDE-style class definition
header (e.g. ===). Exit the mode with enter, or anything that is
not a character, backspace, delete, left or right."
  (interactive)
  (bde-insert-class-header ?=))

(defun bde-insert-declare-class-header ()
  "Mini-mode for creating a BDE-style class implementation header (e.g. ---).
Exit the mode with enter, or anything that is not a character,
backspace, delete, left or right."
  (interactive)
  (bde-insert-class-header ?-))

;;; Ctrl-C = and Ctrl-C - for class header
(global-set-key [(control c)(=)] 'bde-insert-define-class-header)
(global-set-key [(control c)(-)] 'bde-insert-declare-class-header)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BDE's right style comments such as // RETURN or // LOCK

(defun bde-aligh-right-after-point ()
  "Set the right amount of spaces around the point so the text
  after point is right-aligned (for things such as // RETURN). It
  works even if point is in a C++ comment."
  (interactive)
  ;; If we are in the middle of a comment, move point before the comment.
  (re-search-backward "//" (point-at-bol) t)
  (let ((right-thing-length 0)
        (num-spaces         0))
    ;; Remove all spaces around point and calculate the length of the text on
    ;; the right
    (save-excursion
      (delete-horizontal-space)
      (let ((col (current-column)))
        (end-of-line)
        (setq right-thing-length (- (current-column) col))))
    ;; Now insert as many spaces as needed
    (setq num-spaces (- 79 right-thing-length (current-column)))
    (if (> num-spaces 0)
        (dotimes (i num-spaces)
          (insert " "))
      (message "Sorry, not enough space..."))))

;;; Ctrl-> to right-aligh the text after point
(global-set-key [(control >)] 'bde-aligh-right-after-point)


;;; End of file
(provide 'init-bde-style)
