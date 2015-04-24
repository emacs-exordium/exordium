;;; bde-style.el --- Provide BDE-style C++ indentation
;;;
;;; See https://github.com/bloomberg/bde
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c =          `bde-insert-define-class-header'
;;; C-c -          `bde-insert-declare-class-header'
;;; C->            `bde-aligh-right-after-point': align text after point
;;;                to the right. If line ends with a comment such as // RETURN
;;;                or // LOCK, align the comment.
;;; C-c i          `bde-insert-redundant-include-guard'
;;; C-c a          `bde-align-functions-arguments'
;;; C-c f          `bde-align-funcall'
;;; C-c m          `bde-align-class-members'
;;; (no key)       `bde-repunctuate'
;;; -------------- -------------------------------------------------------
;;;
;;; `bde-aligh-right-after-point':
;;; Before (cursor anywhere after semi-colon):
;;;     return x; // RETURN
;;; After:
;;;     return x;                                     // RETURN
;;;
;;; `bde-insert-redundant-include-guard':
;;; Before (cursor must be on the line, or region with one or more includes
;;; must be selected):
;;;     #include <bsl_iostream.h>
;;; After:
;;;     #ifndef INCLUDED_BSL_IOSTREAM
;;;     #include <bsl_iostream.h>
;;;     #endif
;;;
;;; `bde-align-functions-arguments': align function signature
;;; Before (cursor must be inside the argument list):
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
;;; TODO: apparently default values should be aligned too.
;;;
;;; `bde-align-funcall': align function call arguments
;;; Before (cursor must be inside the argument list):
;;;    bslma::ManagedPtr<BufferedMessage> message(
;;;       groupInfo->bufferedMessages()[0], &d_bufferedMessagePool);
;;; After:
;;;    bslma::ManagedPtr<BufferedMessage> message(
;;;                                           groupInfo->bufferedMessages()[0],
;;;                                           &d_bufferedMessagePool);
;;; TODO: currently does not work well with comments.
;;;
;;; `bde-align-class-members'
;;; Before (region must be selected):
;;;    bslma::Allocator *d_allocator_p; // held not owned
;;;    bool d_started;
;;;    bdet_TimeInterval d_idleCheckInterval; // Delay between 2 checks for
;;;         // idle sessions. Default is 0, meaning this feature is not used.
;;; After:
;;;    bslma::Allocator *d_allocator_p;    // held not owned
;;;
;;;    bool              d_started;
;;;
;;;    bdet_TimeInterval d_idleCheckInterval;
;;;                                        // Delay between 2 checks for
;;;                                        // idle sessions. Default is 0,
;;;                                        // meaning this feature is not used.
;;;
;;; `bde-repunctuate': puts two spaces at the end of each sentence in selected
;;; region or comment block.
;;; TODO: the regex should be fixed for words like "e.g." or "i.e.".

(with-no-warnings (require 'cl))
(require 'cc-defs)
(require 'cc-vars)
(require 'cc-mode)
(require 'init-lib)


;;; Utility functions

(defun bde-component-name ()
  "Return the name of the component for the current buffer"
  (let ((name (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name)))))
    (if (pg/string-ends-with name ".t")
        (substring name 0 (- (length name) 2))
      name)))

(defun bde-package-name ()
  "Return the name of the package for the current buffer"
  (interactive)
  (let ((component-name (bde-component-name)))
    (substring component-name 0 (string-match "_" component-name))))


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
                 (forward-line -1))
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

(defun bde-statement-block-intro-offset (element)
  "Custom line-up function for first line of a statement block.
The default identation is is '+' (1 basic offset), unless we are in
a switch statement, in which case the indentation is set to
'*' (half basic offset). Example:
switch(val) {
  case 100: {
      return 1;
  } break;
  default: {
      return 0;
  } break;
}"
  (save-excursion
    (goto-char (c-langelem-pos element))
    (if (looking-at "\\(case\\|default\\)")
        '*
      '+)))

;; See http://cc-mode.sourceforge.net/html-manual/Syntactic-Symbols.html#Syntactic-Symbols
(c-add-style
 "bde"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset . 0)
   (fill-column . 79)
   (c-backslash-column . 78)
   (c-backslash-max-column . 78)
   (c-offsets-alist
    (comment-intro         . bde-comment-offset)
    (defun-open            . 0)
    (defun-close           . 0)
    (statement-block-intro . bde-statement-block-intro-offset)
    (substatement-open     . 0)
    (substatement-label    . 0)
    (label                 . 0)
    (access-label          . /)
    (case-label            . *)
    (statement-case-intro  . *)
    (statement-case-open   . 0)
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


;;; Tabs

;;; Use M-i to go to the next 4-character tab position
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Allow tab in Makefile
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))


;;; Insert class header

;;; Note: left-char and right-char only exist in emacs 24, so we use
;;; backward-char and forward-char instead.
(defun bde-insert-class-header (header-char)
  (let ((erase-hint t))
    (cl-flet ((delete-header-char (n)
                (save-excursion
                  (forward-line -1)
                  (end-of-line)
                  (backward-delete-char n)
                  (forward-line 2)
                  (end-of-line)
                  (backward-delete-char n)))
              (center-header ()
                (save-excursion
                  (forward-line -1)
                  (center-line 3))))
      ;; Get started
      (dotimes (i 3)
        (insert "// ")
        (newline))
      (forward-line -2)
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
                  (forward-line -1)
                  (end-of-line)
                  (when erase-hint (insert " "))
                  (insert-char header-char 1)
                  (forward-line 2)
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

(defun bde-guess-class-name ()
  "Return the name of the class or struct that is defined
immediately after the cursor (skipping any previous spaces and
newlines). Return nil if there isn't any struct or class defined."
  (save-excursion
    (beginning-of-line)
    (when (forward-word)
      (backward-word)
      (when (re-search-forward "^\\(class\\|struct\\) " (point-at-eol) t)
        (concat (match-string 1) " " (current-word))))))

(defun bde-insert-define-class-header ()
  "Mini-mode for creating a BDE-style class definition
header (e.g. ===). Exit the mode with enter, or anything that is
not a character, backspace, delete, left or right."
  (interactive)
  (let ((class-name (bde-guess-class-name)))
    (cond (class-name
           (cl-flet ((insert-bar (n)
                       (insert "// ")
                       (dotimes (i n)
                         (insert "="))
                       (insert "\n")))
             (insert-bar (string-width class-name))
             (insert "// " class-name "\n")
             (insert-bar (string-width class-name))
             (forward-line -3)
             (center-line 3)))
          (t
           (bde-insert-class-header ?=)))))

(defun bde-insert-declare-class-header ()
  "Mini-mode for creating a BDE-style class implementation header (e.g. ---).
Exit the mode with enter, or anything that is not a character,
backspace, delete, left or right."
  (interactive)
  (bde-insert-class-header ?-))

;;; Ctrl-C = and Ctrl-C - for class header
(global-set-key [(control c)(=)] 'bde-insert-define-class-header)
(global-set-key [(control c)(-)] 'bde-insert-declare-class-header)


;;; BDE's right style comments such as // RETURN or // LOCK

(defun bde-aligh-right ()
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
(global-set-key [(control >)] 'bde-aligh-right)


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
            (goto-char (point-min))
            (let ((more-lines t))
              (while more-lines
                (when (or (pg/string-starts-with (thing-at-point 'line) "#ifndef")
                          (pg/string-starts-with (thing-at-point 'line) "#endif"))
                  (kill-whole-line))
                (setq more-lines (= 0 (forward-line 1)))))
            ;; Delete all blank lines
            (goto-char (point-min))
            (flush-lines "^$")
            ;; Sort the buffer, because we want our includes sorted
            (mark-whole-buffer)
            (sort-lines nil (region-beginning) (region-end))
            ;; Add the include guards, line by line
            (goto-char (point-min))
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


;;; Align stuff

(defun bde-max-column-in-region ()
  "Return the largest column in region"
  (let ((m 0))
    (while (< (point) (region-end))
      (end-of-line)
      (setq m (max m (current-column)))
      (forward-line))
    m))

;;; Align arguments in a function declaration

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
  "Assuming the cursor is within the argument list of a function
declaration or definition, align the type and variable names"
  (interactive)
  ;; Get a list of the arguments. Note that the external parentheses are included
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
              (message "Longest line is %d chars" longest-length))))))
    ;; Leave the cursor after the closing parenthese instead of on the opening
    ;; one, since most likely we want to add code after the arg list.
    (when (looking-at "\\s\(")
      (forward-list 1))))

(define-key c-mode-base-map [(control c)(a)] 'bde-align-functions-arguments)

;;; Align arguments in a function call

(defun bde-align-funcall ()
  "Assuming the cursor is within the list of arguments of a
  function call, align the arguments. It puts one argument per
  line and aligns to the right"
  (interactive)
  ;; Get the argument string (remove the external parentheses)
  (let ((arglist (thing-at-point 'list)))
    (when (pg/string-starts-with arglist "(")
      (setq arglist (substring arglist 1)))
    (when (pg/string-ends-with arglist ")")
      (setq arglist (substring arglist 0 (1- (length arglist)))))
    ;; Extract the list of arguments
    (let ((args (split-string arglist ","))
          (parsed-args ()))
      (dolist (arg args)
        (let ((parsed-arg (pg/string-trim (if (pg/string-starts-with arg "\n")
                                              (substring arg 1)
                                            arg))))
          (setq parsed-args (cons parsed-arg parsed-args))))
      (setq parsed-args (reverse parsed-args))
      ;; Cut the argument list and edit it into a temporary buffer
      (backward-up-list)
      (push-mark)
      (forward-list)
      (delete-region (region-beginning) (region-end))
      (insert
       (with-temp-buffer
         (insert "(")
         (let ((i 1))
           (dolist (arg parsed-args)
             (insert arg)
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
                (message "Longest line is %d chars" longest-length)))))))))

(define-key c-mode-base-map [(control c)(f)] 'bde-align-funcall)

;;; Align members in a class

(defun bde-members-list (text)
  "Return a list of members found in text, with each member being
a list (type variable num-stars comment), where num-stars is the
number of * before the variable name, and comment may not be
present. For example:
    int             d_count;       // counter
    bsl::Allocator *d_allocator_p; // held not owned
will return:
    (('int' 'd_count' 0 ' counter')
     ('bsl::Allocator' '*d_allocator_p' 1 ' held not owned'))
There can be more than one comment string in a sublist if comments
include semicolons."
  (cl-flet ((trim (s)
              ;; Returns a trimed 's' or nil if 's' becomes empty
              (let ((trimmed (pg/string-trim s)))
                (if (string= "" trimmed) nil (list trimmed)))))
    ;; Split the text around semicolons, and trim all elements.
    ;; An element will be like 'int d_count' or
    ;; '// counter\n    bsl::Allocator *d_allocator_p'.
    (let ((elements (mapcan #'trim (split-string text ";")))
          (members ()))
      (dolist (element elements)
        ;; Guess the variable, the type, and the number of *.
        (let* ((var-pos    (or (string-match "[_a-z]+[0-9]*\\'" element) 0))
               (var        (substring element var-pos))
               (before-var (substring element 0 var-pos))
               (star-pos   (string-match "\\*[\\s-]*" before-var))
               (star2-pos  (string-match "\\*\\*[\\s-]*" before-var))
               (type       (pg/string-trim
                            (substring before-var 0 (or star-pos var-pos))))
               (comment    nil))
          (when (and members ; e.g. not the first element
                     (pg/string-starts-with element "//"))
            ;; If the element starts with //, there is a comment that belongs
            ;; to the previous element. Also the type must be re-guessed to
            ;; remove any line starting with //
            (let ((type-lines (mapcan #'trim (split-string type "\n"))))
              (setq type (mapconcat #'(lambda (s)
                                        (if (pg/string-starts-with s "//") "" s))
                                    type-lines
                                    "")
                    comment (mapconcat #'(lambda (s)
                                           (if (pg/string-starts-with s "//")
                                               (substring s 2)
                                             ""))
                                       (mapcan #'trim (split-string element "\n"))
                                       ""))))
          ;; Add the comment to the previous element.
          (when comment
            (nconc (car members) (list comment)))
          ;; We may have an empty type if this is the last comment in the
          ;; region. If we have a type, add a new member.
          (unless (string= type "")
            (setq members (cons (list type
                                      (cond (star2-pos (concat "**" var))
                                            (star-pos  (concat "*" var))
                                            (t         var))
                                      (cond (star2-pos 2)
                                            (star-pos  1)
                                            (t         0)))
                                members)))))
      ;; Result
      (reverse members))))

(defun bde-guess-indentation-level ()
  "Return the number of spaces needed for correct indentation at
  point"
  (let ((n 0))
    (save-excursion
      ;; There is probably a better way to do that...
      (newline)
      (forward-line -1)
      (indent-according-to-mode)
      (setq n (current-indentation))
      (beginning-of-line)
      (kill-line))
    n))

(defun bde-align-class-members ()
  "Assuming a region is selected containing class members, align
these members according to the BDE style. Note that all comments
start at column 40."
  (interactive)
  (cond ((use-region-p)
         ;; Parse the region and get a list of members.
         ;; Each member is a list (type variable num-stars comments).
         ;; There are 0 to many comments.
         (let ((members (bde-members-list
                         (buffer-substring (region-beginning) (region-end))))
               (max-type-length 0)
               (max-stars 0)
               (num-spaces 0))
           ;; Get the max type length and the max number of *
           (dolist (member members)
             (let ((type      (car member))
                   (num-stars (caddr member)))
               (setq max-type-length (max max-type-length (length type))
                     max-stars (max max-stars num-stars))))
           ;; Cut and reformat the region
           (save-excursion
             (delete-region (region-beginning) (region-end))
             (setq num-spaces (bde-guess-indentation-level))
             (insert
              (with-temp-buffer
                (let ((num-members (length members)))
                  (dolist (member members)
                    (let ((type      (car member))
                          (var       (cadr member))
                          (num-stars (caddr member))
                          (comments  (cdddr member)))
                      ;; Insert type and variable
                      (insert-char ?\s num-spaces)
                      (insert type)
                      (insert (make-string (+ (- max-type-length (length type))
                                              (- max-stars num-stars)
                                              1) ; at least one space
                                           ?\s))
                      (insert var)
                      (insert ";")
                      (when comments
                        ;; Insert comments on a new line if the current column > 40
                        (when (>= (current-column) 40)
                          (newline))
                        (insert (make-string (- 40 (current-column)) ?\s))
                        (insert "//")
                        (dolist (comment comments)
                          (insert comment)))
                      ;; One blank line between two members
                      (decf num-members)
                      (newline (if (> num-members 0) 2 1))))
                  (buffer-string))))
             ;; Fix the comments for the 79th column, e.g. fill-paragraph on each
             (forward-line -1)
             (dolist (member (reverse members))
               ;; Move back, skipping empty lines
               (while (= (point-at-bol) (point-at-eol))
                 (forward-line -1))
               (let ((comments (cdddr member)))
                 (when comments
                   (end-of-line)
                   (c-fill-paragraph)))
               ;; Move back until we find empty line
               (while (not (= (point-at-bol) (point-at-eol)))
                 (forward-line -1))))))
        (t
         (message "No region"))))

(define-key c-mode-base-map [(control c)(m)] 'bde-align-class-members)


;;; Repunctuate: the BDE comment style requires 2 spaces at the end of each
;;; sentence, which is both annoying and debatable:
;;; http://en.wikipedia.org/wiki/Sentence_spacing
;;; Fortunately Emacs can take care of that for us.

(defun bde-in-comment-p ()
  "Predicate returning non-nil if the cursor is within a C++ comment."
  (let ((syntax (if (boundp 'c-syntactic-context)
                    ;; Use `c-syntactic-context' in the same way as
                    ;; `c-indent-line', to be consistent.
                    c-syntactic-context
                  (c-save-buffer-state nil
                    (c-guess-basic-syntax)))))
    ;; `syntax' is c-syntactic-context and contains 'comment-intro if
    ;; we are within a comment block.
    (assq 'comment-intro syntax)))

(defun bde-comment-beginning ()
  "Return the position of the beginning of a comment block, at
the beginning of the first line, or nil if not found. It is safer
to use this function in conjunction with `bde-in-comment-p'. Note
that this function only considers lines that only contain a
comment."
  (loop
   (beginning-of-line)
   (cond ((= (point) (point-min))
          (return nil))
         ((re-search-forward "^ *//" (point-at-eol) t)
          ;; looking at a comment line
          (forward-line -1))
         (t
          (forward-line 1)
          (return (point))))))

(defun bde-comment-end ()
  "Return the position of the end of a comment block, at the end
of the last line, or nil if not found. It is safer to use this
function in conjunction with `bde-in-comment-p'. Note that this
function only considers lines that only contain a comment."
  (loop
   (beginning-of-line)
   (cond ((= (point) (point-min))
          (return nil))
         ((re-search-forward "^ *//" (point-at-eol) t)
          ;; looking at a comment line
          (forward-line 1))
         (t
          (forward-line -1)
          (end-of-line)
          (return (point))))))

(defun bde-repunctuate ()
  "Put two spaces at the end of sentences in the selected region
or comment block. See also `repunctuate-sentences'."
  (interactive)
  (let (beginning end)
    (cond ((region-active-p)
           (setq beginning (region-beginning)
                 end (region-end)))
          ((bde-in-comment-p)
           (setq beginning (bde-comment-beginning)
                 end (bde-comment-end))))
    (if (and beginning end)
        (save-excursion
          (goto-char beginning)
          (while (re-search-forward "\\([]\"')]?\\)\\([.?!]\\)\\([]\"')]?\\) +" end t)
            (replace-match "\\1\\2\\3  " nil nil))
          (fill-paragraph))
      (message "No region or comment"))))


(provide 'init-bde-style)
