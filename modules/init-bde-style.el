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
;;; C-c i          `bde-insert-redundant-include-guard'
;;; C-c a          `bde-align-functions-arguments'
;;;
;;; Aligh right after point:
;;; Before:
;;;     return x; <cursor> // RETURN
;;; After:
;;;     return x;                                     // RETURN
;;;
;;; Insert redundant include guard (cursor must be on the line):
;;; Before:
;;;     #include <bsl_iostream.h>
;;; After:
;;;     #ifndef INCLUDED_BSL_IOSTREAM
;;;     #include <bsl_iostream.h>
;;;     #endif
;;;
;;; Align function arguments (cursor must be inside argument list):
;;; Before:
;;;    Customer(const BloombergLP::bslstl::StringRef& firstName,
;;;             const BloombergLP::bslstl::StringRef& lastName,
;;;             const bsl::vector<int>& accounts,
;;;             int id,
;;;             BloombergLP::bslma::Allocator *basicAllocator = 0);
;;; After:
;;;    Customer(const BloombergLP::bslstl::StringRef&  firstName,
;;;             const BloombergLP::bslstl::StringRef&  lastName,
;;;             const bsl::vector<int>&                accounts,
;;;             int                                    id,
;;;             BloombergLP::bslma::Allocator         *basicAllocator = 0);

(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;
;;; This section define a C style named "bde" using c-add-style.  The offset
;;; in the specification (c-offset-alist) can be any of the following:
;;;
;;; - An integer -> specifies a relative offset. All relative offsets will be
;;;   added together and used to calculate the indentation relative to an
;;;   anchor position earlier in the buffer.
;;; - One of the symbols +, -, ++, --, *, or /
;;;   +   = c-basic-offset times 1
;;;   -   = c-basic-offset times −1
;;;   ++  = c-basic-offset times 2
;;;   --  = c-basic-offset times −2
;;;   *   = c-basic-offset times 0.5
;;;   /   = c-basic-offset times −0.5
;;;
;;; Note: to debug the indentation of a particular line, type 'C-c C-s'. It
;;; will display the variable 'c-syntactic-context' which is a list of the
;;; syntactic components affect the offset calculations for that line, with the
;;; character position in the buffer for each of them. More details in M-x
;;; info, then CC mode, then Interactive Customization.
;;; See cc-align.el for examples of line-up functions.

(defun bde-comment-offset (element)
  "Custom line-up function for BDE comments.
Return a symbol for the correct indentation level at the
current cursor position, if the cursor is within a class definition:
1. + for method comments:
        int foo() const = 0;
            // tab goes here
        int bar() { return 0; }
            // tab goes here
2. column number of beginning of comment for data member comments:
        int d_data;     // my comment at whatever column I want
                        // tab goes here
        int d_someLongVariableName;
                        // my comment at whatever column I want
                        // tab goes here
3. nil otherwise."
  (case (caar c-syntactic-context)
    ((inclass innamespace)
     (save-excursion
       (let ((class-offset         ; extra offset for inner structs
              (c-langelem-col (car c-syntactic-context) t))
             (comment-column nil)) ; column number of last //
         (loop
          (beginning-of-line)
          (cond ((= (point) (point-min))
                 (return nil))
                ((re-search-forward "^ *//" (point-at-eol) t)
                 ;; looking at a comment line
                 (setq comment-column (- (current-column) 2))
                 (next-line -1))
                ((re-search-forward ") *\\(const\\)? *\\(= *0\\)? *; *$"
                                    (point-at-eol) t)
                 ;; looking at end of method declaration
                 (return '+))
                ((re-search-forward "} *$" (point-at-eol) t)
                 ;; looking at end of inline method definition
                 (return '+))
                ((re-search-forward "; *//" (point-at-eol) t)
                 ;; looking at beginning of data member comment block
                 (return (- (current-column) 2 class-offset c-basic-offset)))
                ((and comment-column
                      (re-search-forward "[_A-Za-z0-9]+; *$"
                                         (point-at-eol) t))
                 ;; looking at end of (long?) data member declaration
                 (return (- comment-column class-offset c-basic-offset)))
                (t
                 (return nil)))))))
    (t nil)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabs

;;; Use M-i to go to the next 4-character tab position
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Allow tab in Makefile
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert class header

;;; Note: left-char and right-char only exist in emacs 24, so we use
;;; backward-char and forward-char instead.
(defun bde-insert-class-header (header-char)
  (let ((erase-hint t))
    (cl-flet ((delete-header-char (n)
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
                  (end-of-line)
                  (when erase-hint (insert " "))
                  (insert-char header-char 1)
                  (next-line 2)
                  (end-of-line)
                  (when erase-hint (insert " "))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert redundant include guards

(defun bde-insert-redundant-include-guard ()
  "If the current line is a #include, inserts a redundant include
guard around it"
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (cond ((string-match "^#include <[_\.a-z0-9]+>$" current-line)
           (let ((file-name (substring current-line 10 -2)))
             (when (pg/string-ends-with file-name ".h")
               (setq file-name (substring file-name 0 -2)))
             (save-excursion
               (beginning-of-line)
               (insert "#ifndef INCLUDED_" (upcase file-name) "\n")
               (forward-line 1)
               (when (> (current-column) 0) (insert "\n"))
               (insert "#endif\n"))))
          (t
           (message "Not on a #include line")))))

(defun bde-insert-redundant-include-guard-region ()
  "If a region of #include lines is selected, sort them and add
any missing redundant include guards. If no region is selected
and the current line is a #include, insert a redundant include
guard around it"
  (interactive)
  (cond ((use-region-p)
         (kill-region (region-beginning) (region-end))
         (insert
          (with-temp-buffer
            (yank)
            ;; Remove any existing include guard and blank line
            (beginning-of-buffer)
            (let ((more-lines t))
              (while more-lines
                (when (or (pg/string-starts-with (thing-at-point 'line) "#ifndef")
                          (pg/string-starts-with (thing-at-point 'line) "#endif"))
                  (kill-whole-line))
                (setq more-lines (= 0 (forward-line 1)))))
            ;; Delete all blank lines
            (beginning-of-buffer)
            (flush-lines "^$")
            ;; Sort the buffer, because we want our includes sorted
            (mark-whole-buffer)
            (sort-lines nil (region-beginning) (region-end))
            ;; Add the include guards, line by line
            (beginning-of-buffer)
            (let ((more-lines t))
              (while more-lines
                (insert "\n") ; insert a blank line between 2 includes
                (bde-insert-redundant-include-guard)
                (forward-line 2) ; move to after #endif
                (setq more-lines (= 0 (forward-line 1)))))
            (buffer-string))))
        (t
         (bde-insert-redundant-include-guard))))

(define-key c-mode-base-map [(control c)(i)] 'bde-insert-redundant-include-guard-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Align function arguments

(defun bde-max-column-in-region ()
  "Return the largest column in region"
  (let ((m 0))
    (while (< (point) (region-end))
      (end-of-line)
      (setq m (max m (current-column)))
      (forward-line))
    m))

(defun bde-parse-argument (argument)
  "Parse the argument line (which may include the external
  parentheses, but should not include comas), and return a list of:
  - the type expression which may be empty,
  - the name of the variable with any * or ** concatenated,
  - the number of * (0, 1 or 2)
  - the default value expression or nil.
  For example 'int* p = 0' will return ('int' '*p' '= 0').
  All these strings are trimmed."
  (when (pg/string-starts-with argument "(")
    (setq argument (substring argument 1)))
  (when (pg/string-ends-with argument ")")
    (setq argument (substring argument 0 (1- (length argument)))))
  (let* ((assign-pos    (string-match "=" argument))
         (assign        (when assign-pos
                          (pg/string-trim (substring argument assign-pos))))
         (before-assign (if assign-pos
                            (substring argument 0 assign-pos)
                          argument))
         (var-pos       (or (string-match "[_a-z]+[0-9]*$"
                                          (pg/string-trim-end before-assign))
                            0))
         (var           (pg/string-trim (substring before-assign var-pos)))
         (before-var    (substring argument 0 var-pos))
         (star-pos      (string-match "\\*[\\s-]*" before-var))
         (star2-pos     (string-match "\\*\\*[\\s-]*" before-var))
         (type          (pg/string-trim
                         (substring before-var 0 (or star-pos var-pos)))))
    (list type
          (cond (star2-pos (concat "**" var))
                (star-pos  (concat "*" var))
                (t         var))
          (cond (star2-pos 2)
                (star-pos  1)
                (t         0))
          assign)))

(defun bde-align-functions-arguments ()
  "Assuming the cursor is within a function's argument list,
  align them"
  (interactive)
  ;; Get a list of the arguments. Note that the externam parentheses are included
  (let ((args (split-string (thing-at-point 'list) ","))
        (parsed-args ())
        (max-type-length 0)
        (max-stars 0))
    ;; Parse each argument and get the max type length
    (dolist (arg args)
      (let ((parsed-arg (bde-parse-argument arg)))
        (setq parsed-args (cons parsed-arg parsed-args)
              max-type-length (max max-type-length (length (car parsed-arg)))
              max-stars (max max-stars (caddr parsed-arg)))))
    (setq parsed-args (reverse parsed-args))
    ;; Cut the argument list and edit it into a temporary buffer
    (backward-up-list)
    (push-mark)
    (forward-list)
    (kill-region (region-beginning) (region-end))
    (insert
     (with-temp-buffer
       (insert "(")
       (let ((i 1)
             (parsed-arg nil))
         (dolist (parsed-arg parsed-args)
           (let ((type      (car parsed-arg))
                 (var       (cadr parsed-arg))
                 (num-stars (caddr parsed-arg))
                 (assign    (cadddr parsed-arg)))
             (when (> (length type) 0)
               (insert type)
               (insert (make-string (+ (- max-type-length (length type))
                                       (- max-stars num-stars)
                                       1) ; at least one space
                                    ?\s)))
             (insert var)
             (when assign
               (insert " " assign)))
           (unless (>= i (length parsed-args))
             (insert ",")
             (newline))
           (incf i)))
       (insert ")")
       (buffer-string)))
    ;; Reindent
    (push-mark)
    (backward-list)
    (indent-region (region-beginning) (region-end))
    ;; If some lines exceed the dreadful 79th column, insert a new line before
    ;; the first line and reindent, with longest line to the right edge
    (save-excursion
      (let ((start-col (1+ (current-column)))
            (max-col (bde-max-column-in-region)))
        (when (> max-col 79)
          (backward-list)
          (forward-char)
          (newline)
          (backward-char 2)
          (push-mark)
          (forward-list)
          (let ((longest-length (- max-col start-col)))
            (if (<= longest-length 79)
                (indent-region (region-beginning) (region-end)
                               (- 79 longest-length))
              ;; We cannot indent correctly, some lines are too long
              (indent-region (region-beginning) (region-end))
              (message "Longest line is %d chars" longest-length))))))))

(define-key c-mode-base-map [(control c)(a)] 'bde-align-functions-arguments)


;;; End of file
(provide 'init-bde-style)
