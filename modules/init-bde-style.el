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
;;; Note that you need to have the & and * at the right places. It will fail
;;; with things like:
;;;    int* foo                         // * not before foo
;;;    int * foo                        // spaces between * and foo
;;;    const Foo &foo                   // & not right after type
;;;    bslma::Allocator *allocator=0    // no spaces before = and 0

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert redundant include guards

(defun bde-insert-redundant-include-guard ()
  "If the current line is a #include, inserts a redundant include
guard around it"
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (cond ((string-match "^#include <[_\.a-z]+>$" current-line)
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
                (delete-blank-lines)
                (setq more-lines (= 0 (forward-line 1)))))
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

(defun bde-calculate-type-length (words)
  "Calculate the length of the type in a function argument
  represented by a list of words like (int foo)"
  (cond ((or (equal (car words) "(const")
             (equal (car words) "const"))
         (+ (length "const") (length (cadr words)) 1))
        (t
         (if (pg/string-starts-with (car words) "(")
             (1- (length (car words)))
           (length (car words))))))

(defun bde-default-value-p (words)
  "Check if the function arguments represented by a list of words
  like (int *foo = 0) includes a default value"
  (equal (nth (- (length words) 2) words) "="))

(defun bde-pointer-p (words)
  "Check if the function argument represented by a list of words
  like (int *foo = 0) is a pointer"
  (let ((variable-name (if (bde-default-value-p words)
                           (nth (- (length words) 3) words)
                         (car (last words)))))
    (pg/string-starts-with variable-name "*")))

(defun bde-align-functions-arguments ()
  "Assuming the cursor is within a function's argument list,
  align them"
  (interactive)
  ;; Get a list of the arguments. Note that the parentheses are included
  (let ((arguments (split-string (thing-at-point 'list) ","))
        (type-lengths ())
        (max-type-length 0)
        (pointers ())
        (has-pointer nil)
        (default-values ()))
    ;; Parse each argument to get the length of its type, and keep track of the
    ;; longuest type, the pointers and the default values
    (dolist (arg arguments)
      (let* ((words (split-string arg))
             (len (bde-calculate-type-length words))
             (is-pointer (bde-pointer-p words))
             (is-default-value (bde-default-value-p words)))
        (assert (>= (length words) 2))
        (setq type-lengths (cons len type-lengths)
              max-type-length (max len max-type-length)
              pointers (cons is-pointer pointers)
              has-pointer (or has-pointer is-pointer)
              default-values (cons is-default-value default-values))))
    (setq type-lengths (reverse type-lengths)
          pointers (reverse pointers)
          default-values (reverse default-values))
    ;; Cut the argument list and edit it into a temporary buffer
    (backward-up-list)
    (push-mark)
    (forward-list)
    (kill-region (region-beginning) (region-end))
    (insert
     (with-temp-buffer
       (let ((i 0))
         (dolist (arg arguments)
           ;; Copy the argument line
           (insert arg)
           (incf i)
           (unless (= i (length arguments))
             (insert ","))
           ;; Adjust the spaces in this line (we're at end of line)
           (forward-whitespace -1)
           (when (car default-values)
             (forward-whitespace -2))
           (delete-horizontal-space)
           (insert
            (make-string (+ (- max-type-length (car type-lengths))
                            (if (and has-pointer (not (car pointers))) 1 0)
                            1) ; at least one space
                         ?\s))
           (setq type-lengths (cdr type-lengths)
                 pointers (cdr pointers)
                 default-values (cdr default-values))
           (end-of-line)))
       (buffer-string)))))

(define-key c-mode-base-map [(control c)(a)] 'bde-align-functions-arguments)


;;; End of file
(provide 'init-bde-style)
