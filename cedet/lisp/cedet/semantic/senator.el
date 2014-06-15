;;; semantic/senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000-2013 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: FSF
;; Created: 10 Nov 2000
;; Keywords: syntax

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines some user commands for navigating between
;; Semantic tags.  This is a subset of the version of senator.el in
;; the upstream CEDET package; the rest is incorporated into other
;; parts of Semantic or Emacs.

;;; Code:

(require 'ring)
(require 'working)
(require 'semantic)
(require 'semantic/ctxt)
(require 'semantic/decorate)
(require 'semantic/format)
(require 'semantic/db)
(require 'semantic/db-find)


(eval-when-compile (require 'semantic/find))

;; (eval-when-compile (require 'hippie-exp))

(declare-function semantic-analyze-tag-references "semantic/analyze/refs")
(declare-function semantic-analyze-refs-impl "semantic/analyze/refs")
(declare-function semantic-analyze-find-tag "semantic/analyze")
(declare-function semantic-analyze-tag-type "semantic/analyze/fcn")
(declare-function semantic-tag-external-class "semantic/sort")
(declare-function imenu--mouse-menu "imenu")

;;; Customization
(defgroup senator nil
  "Semantic Navigator."
  :group 'semantic)

(defvar senator-minor-mode nil
  "Non-nil if Senator minor mode is enabled.
Use the command `senator-minor-mode' to change this variable.")
(make-variable-buffer-local 'senator-minor-mode)

;;;###autoload
(defcustom senator-step-at-tag-classes nil
  "List of tag classes recognized by Senator's navigation commands.
A tag class is a symbol, such as `variable', `function', or `type'.

As a special exception, if the value is nil, Senator's navigation
commands recognize all tag classes."
  :group 'senator
  :type '(repeat (symbol)))
;;;###autoload
(make-variable-buffer-local 'senator-step-at-tag-classes)

;;;###autoload
(defcustom senator-step-at-start-end-tag-classes nil
  "List of tag classes at which Senator's navigation commands should stop.
A tag class is a symbol, such as `variable', `function', or `type'.
The navigation commands stop at the start and end of each tag
class in this list, provided the tag class is recognized (see
`senator-step-at-tag-classes').

As a special exception, if the value is nil, the navigation
commands stop at the beginning of every tag.

If t, the navigation commands stop at the start and end of any
tag, where possible."
  :group 'senator
  :type '(choice :tag "Identifiers"
                 (repeat :menu-tag "Symbols" (symbol))
                 (const  :tag "All" t)))
;;;###autoload
(make-variable-buffer-local 'senator-step-at-start-end-tag-classes)

(defcustom senator-highlight-found nil
  "If non-nil, Senator commands momentarily highlight found tags."
  :group 'senator
  :type 'boolean)
(make-variable-buffer-local 'senator-highlight-found)

;;; Faces
(defface senator-momentary-highlight-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray70")))
  "Face used to momentarily highlight tags."
  :group 'semantic-faces)

;;; Common functions

(defun senator-momentary-highlight-tag (tag)
  "Momentarily highlight TAG.
Does nothing if `senator-highlight-found' is nil."
  (and senator-highlight-found
       (semantic-momentary-highlight-tag
        tag 'senator-momentary-highlight-face)))

(defun senator-step-at-start-end-p (tag)
  "Return non-nil if must step at start and end of TAG."
  (and tag
       (or (eq senator-step-at-start-end-tag-classes t)
           (memq (semantic-tag-class tag)
                 senator-step-at-start-end-tag-classes))))

(defun senator-skip-p (tag)
  "Return non-nil if must skip TAG."
  (and tag
       senator-step-at-tag-classes
       (not (memq (semantic-tag-class tag)
                  senator-step-at-tag-classes))))

(defun senator-middle-of-tag-p (pos tag)
  "Return non-nil if POS is between start and end of TAG."
  (and (> pos (semantic-tag-start tag))
       (< pos (semantic-tag-end   tag))))

(defun senator-step-at-parent (tag)
  "Return TAG's outermost parent if must step at start/end of it.
Return nil otherwise."
  (if tag
      (let (parent parents)
        (setq parents (semantic-find-tag-by-overlay
                       (semantic-tag-start tag)))
        (while (and parents (not parent))
          (setq parent  (car parents)
                parents (cdr parents))
          (if (or (eq tag parent)
                  (senator-skip-p parent)
                  (not (senator-step-at-start-end-p parent)))
              (setq parent nil)))
        parent)))

(defun senator-previous-tag-or-parent (pos)
  "Return the tag before POS or one of its parent where to step."
  (let (ol tag)
    (while (and pos (> pos (point-min)) (not tag))
      (setq pos (semantic-overlay-previous-change pos))
      (when pos
        ;; Get overlays at position
        (setq ol (semantic-overlays-at pos))
        ;; find the overlay that belongs to semantic
        ;; and STARTS or ENDS at the found position.
        (while (and ol (not tag))
          (setq tag (semantic-overlay-get (car ol) 'semantic))
          (unless (and tag (semantic-tag-p tag)
                       (or (= (semantic-tag-start tag) pos)
                           (= (semantic-tag-end   tag) pos)))
            (setq tag nil
                  ol (cdr ol))))))
    (or (senator-step-at-parent tag) tag)))

(defun senator-full-tag-name (tag parent)
  "Compose a full name from TAG name and PARENT names.
That is append to TAG name PARENT names each one separated by
`semantic-type-relation-separator-character'.  The PARENT list is in
reverse order."
  (let ((sep  (car semantic-type-relation-separator-character))
        (name ""))
    (while parent
      (setq name (concat name sep
                         (semantic-tag-name (car parent)))
            parent (cdr parent)))
    (concat (semantic-tag-name tag) name)))
(semantic-alias-obsolete 'senator-full-token-name
                         'senator-full-tag-name nil)

(defvar senator-completion-cache nil
  "The latest full completion list is cached here.")
(make-variable-buffer-local 'senator-completion-cache)

(defun senator-completion-cache-flush-fcn (&optional ignore)
  "Hook run to clear the completion list cache.
It is called each time the semantic cache is changed.
IGNORE arguments."
  (setq senator-completion-cache nil))

(defun senator-completion-flatten-stream (stream parents &optional top-level)
  "Return a flat list of all tags available in STREAM.
PARENTS is the list of parent tags.  Each element of the list is a
pair (TAG . PARENTS) where PARENTS is the list of TAG parent
tags or nil.  If TOP-LEVEL is non-nil the completion list will
contain only tags at top level.  Otherwise all component tags are
included too."
  (let (fs e tag components)
    (while stream
      (setq tag  (car stream)
            stream (cdr stream)
            e      (cons tag parents)
            fs     (cons e fs))
      (and (not top-level)
           ;; Not include function arguments
           (not (semantic-tag-of-class-p tag 'function))
           (setq components (semantic-tag-components tag))
           (setq fs (append fs (senator-completion-flatten-stream
                                components e)))))
    fs))

(defun senator-completion-function-args (tag)
  "Return a string of argument names from function TAG."
  (mapconcat #'(lambda (arg)
                 (if (semantic-tag-p arg)
                     (semantic-tag-name arg)
                   (format "%s" arg)))
             (semantic-tag-function-arguments tag)
             semantic-function-argument-separation-character))

(defun senator-completion-refine-name (elt)
  "Refine the name part of ELT.
ELT has the form (NAME . (TAG . PARENTS)).  The NAME refinement is
done in the following incremental way:

- If TAG is a function, append the list of argument names to NAME.

- If TAG is a type, append \"{}\" to NAME.

- If TAG is an include, append \"#\" to NAME.

- If TAG is a package, append \"=\" to NAME.

- If TAG has PARENTS append to NAME, the first separator in
  `semantic-type-relation-separator-character', followed by the next
  parent name.

- Otherwise NAME is set to \"tag-name@tag-start-position\"."
  (let* ((sep     (car semantic-type-relation-separator-character))
         (name    (car elt))
         (tag     (car (cdr elt)))
         (parents (cdr (cdr elt)))
         (oname   (semantic-tag-name tag))
         (class   (semantic-tag-class tag)))
    (cond
     ((and (eq class 'function) (string-equal name oname))
      (setq name (format "%s(%s)" name
                         (senator-completion-function-args tag))))
     ((and (eq class 'type) (string-equal name oname))
      (setq name (format "%s{}" name)))
     ((and (eq class 'include) (string-equal name oname))
      (setq name (format "%s#" name)))
     ((and (eq class 'package) (string-equal name oname))
      (setq name (format "%s=" name)))
     (parents
      (setq name (format "%s%s%s" name
                         (if (semantic-tag-of-class-p
                              (car parents) 'function)
                             ")" sep)
                         (semantic-tag-name (car parents)))
            parents (cdr parents)))
     (t
      (setq name (format "%s@%d" oname
                         (semantic-tag-start tag)))))
    (setcar elt name)
    (setcdr elt (cons tag parents))))

(defun senator-completion-uniquify-names (completion-stream)
  "Uniquify names in COMPLETION-STREAM.
That is refine the name part of each COMPLETION-STREAM element until
there is no duplicated names.  Each element of COMPLETION-STREAM has
the form (NAME . (TAG . PARENTS)).  See also the function
`senator-completion-refine-name'."
  (let ((completion-stream (sort completion-stream
                                 #'(lambda (e1 e2)
                                     (string-lessp (car e1)
                                                   (car e2)))))
        (dupp t)
        clst elt dup name)
    (while dupp
      (setq dupp nil
            clst completion-stream)
      (while clst
        (setq elt  (car clst)
              name (car elt)
              clst (cdr clst)
              dup  (and clst
                        (string-equal name (car (car clst)))
                        elt)
              dupp (or dupp dup))
        (while dup
          (senator-completion-refine-name dup)
          (setq elt (car clst)
                dup (and elt (string-equal name (car elt)) elt))
          (and dup (setq clst (cdr clst))))))
    ;; Return a usable completion alist where each element has the
    ;; form (NAME . TAG).
    (setq clst completion-stream)
    (while clst
      (setq elt  (car clst)
            clst (cdr clst))
      (setcdr elt (car (cdr elt))))
    completion-stream))

(defun senator-completion-stream (stream &optional top-level)
  "Return a useful completion list from tags in STREAM.
That is an alist of all (COMPLETION-NAME . TAG) available.
COMPLETION-NAME is an unique tag name (see also the function
`senator-completion-uniquify-names').  If TOP-LEVEL is non-nil the
completion list will contain only tags at top level.  Otherwise all
sub tags are included too."
  (let* ((fs (senator-completion-flatten-stream stream nil top-level))
         cs elt tag)
    ;; Transform each FS element from (TAG . PARENTS)
    ;; to (NAME . (TAG . PARENT)).
    (while fs
      (setq elt (car fs)
            tag (car elt)
            fs  (cdr fs)
            cs  (cons (cons (semantic-tag-name tag) elt) cs)))
    ;; Return a completion list with unique COMPLETION-NAMEs.
    (senator-completion-uniquify-names cs)))

(defun senator-current-type-context ()
  "Return tags in the type context at point or nil if not found."
  (let ((context (semantic-find-tags-by-class
                  'type (semantic-find-tag-by-overlay))))
    (if context
        (semantic-tag-type-members
         (nth (1- (length context)) context)))))

(defun senator-completion-list (&optional in-context)
  "Return a useful completion list from tags in current buffer.
If IN-CONTEXT is non-nil return only the top level tags in the type
context at point or the top level tags in the current buffer if no
type context exists at point."
  (let (stream)
    (if in-context
        (setq stream (senator-current-type-context)))
    (or stream (setq stream (semantic-fetch-tags)))
    ;; IN-CONTEXT completion doesn't use nor set the cache.
    (or (and (not in-context) senator-completion-cache)
        (let ((clst (senator-completion-stream stream in-context)))
          (or in-context
              (setq senator-completion-cache clst))
          clst))))

(defun senator-find-tag-for-completion (prefix)
  "Find all tags with a name starting with PREFIX.
Uses `semanticdb' when available."
  (let ((tagsa nil))
    (when (and (featurep 'semantic/analyze))
      (let ((ctxt (semantic-analyze-current-context)))
        (when ctxt
          (condition-case nil
              (setq tagsa (semantic-analyze-possible-completions
                           ctxt))
            (error nil)))))

    (if tagsa
        tagsa
      ;; If the analyzer fails, then go into boring completion
      (if (and (featurep 'semantic/db) (semanticdb-minor-mode-p))
	  (semanticdb-fast-strip-find-results
	   ;; semanticdb version returns a list of (DB-TABLE . TAG-LIST)
	   (semanticdb-deep-find-tags-for-completion prefix))
	;; semantic version returns a TAG-LIST
	(semantic-deep-find-tags-for-completion prefix (current-buffer))))))

;;; Senator stream searching functions: no more supported.
;;
(defun senator-find-nonterminal-by-name (&rest ignore)
  (error "Use the semantic and semanticdb find API instead"))

(defun senator-find-nonterminal-by-name-regexp (&rest ignore)
  (error "Use the semantic and semanticdb find API instead"))

;;; Search functions

(defun senator-search-tag-name (tag)
  "Search for TAG name in current buffer.
Limit the search to TAG bounds.
If found, set point to the end of the name, and return point.  The
beginning of the name is at (match-beginning 0).
Return nil if not found, that is if TAG name doesn't come from the
source."
  (let ((name (semantic-tag-name tag)))
    (setq name (if (string-match "\\`\\([^[]+\\)[[]" name)
                   (match-string 1 name)
                 name))
    (goto-char (semantic-tag-start tag))
    (when (re-search-forward (concat
                              ;; The tag name is expected to be
                              ;; between word delimiters, whitespace,
                              ;; or punctuation.
                              "\\(\\<\\|\\s-+\\|\\s.\\)"
                              (regexp-quote name)
                              "\\(\\>\\|\\s-+\\|\\s.\\)")
                             (semantic-tag-end tag)
                             t)
      (goto-char (match-beginning 0))
      (search-forward name))))

(defcustom senator-search-ignore-tag-classes
  '(code block)
  "List of ignored tag classes.
Tags of those classes are excluded from search."
  :group 'senator
  :type '(repeat (symbol :tag "class")))

(defun senator-search-default-tag-filter (tag)
  "Default function that filters searched tags.
Ignore tags of classes in `senator-search-ignore-tag-classes'"
  (not (memq (semantic-tag-class tag)
             senator-search-ignore-tag-classes)))

(defvar senator-search-tag-filter-functions
  '(senator-search-default-tag-filter)
  "List of functions to be called to filter searched tags.
Each function is passed a tag.  If one of them returns nil, the tag is
excluded from the search.")

(defun senator-search (searcher text &optional bound noerror count)
  "Use the SEARCHER function to search from point for TEXT in a tag name.
SEARCHER is typically the function `search-forward', `search-backward',
`word-search-forward', `word-search-backward', `re-search-forward', or
`re-search-backward'.  See one of the above function to see how the
TEXT, BOUND, NOERROR, and COUNT arguments are interpreted."
  (let* ((origin (point))
         (count  (or count 1))
         (step   (cond ((> count 0) 1)
                       ((< count 0) (setq count (- count)) -1)
                       (0)))
         found next sstart send tag tstart tend)
    (or (zerop step)
        (while (and (not found)
                    (setq next (funcall searcher text bound t step)))
          (setq sstart (match-beginning 0)
                send   (match-end 0))
          (if (= sstart send)
              (setq found t)
            (and (setq tag (semantic-current-tag))
                 (run-hook-with-args-until-failure
                  'senator-search-tag-filter-functions tag)
                 (setq tend   (senator-search-tag-name tag))
                 (setq tstart (match-beginning 0)
                       found  (and (>= sstart tstart)
                                   (<= send tend)
                                   (zerop (setq count (1- count))))))
            (goto-char next))))
    (cond ((null found)
           (setq next origin
                 send origin))
          ((= next sstart)
           (setq next send
                 send sstart))
          (t
           (setq next sstart)))
    (goto-char next)
    ;; Setup the returned value and the `match-data' or maybe fail!
    (funcall searcher text send noerror step)))

;;; Navigation commands

;;;###autoload
(defun senator-next-tag ()
  "Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-start tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (semantic-find-tag-by-overlay-next pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (semantic-find-tag-by-overlay-next
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-max))
          (message "End of buffer"))
      (cond ((and (senator-step-at-start-end-p tag)
                  (or (= pos (semantic-tag-start tag))
                      (senator-middle-of-tag-p pos tag)))
             (setq where "end")
             (goto-char (semantic-tag-end tag)))
            (t
             (setq where "start")
             (goto-char (semantic-tag-start tag))))
      (senator-momentary-highlight-tag tag)
      (message "%S: %s (%s)"
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)
	       where))
    tag))

;;;###autoload
(defun senator-previous-tag ()
  "Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (senator-previous-tag-or-parent pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (senator-previous-tag-or-parent
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-min))
          (message "Beginning of buffer"))
      (cond ((or (not (senator-step-at-start-end-p tag))
                 (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag))
             (setq where "start")
             (goto-char (semantic-tag-start tag)))
            (t
             (setq where "end")
             (goto-char (semantic-tag-end tag))))
      (senator-momentary-highlight-tag tag)
      (message "%S: %s (%s)"
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)
	       where))
    tag))
(semantic-alias-obsolete 'senator-previous-token 'senator-previous-tag nil)

(defvar senator-jump-completion-list nil
  "`senator-jump' stores here its current completion list.
Then use `assoc' to retrieve the tag associated to a symbol.")

(defun senator-jump-interactive (prompt &optional in-context no-default require-match)
  "Called interactively to provide completion on some tag name.

Use PROMPT.  If optional IN-CONTEXT is non-nil jump in the local
type's context \(see function `senator-current-type-context').  If
optional NO-DEFAULT is non-nil do not provide a default value.  If
optional REQUIRE-MATCH is non-nil an explicit match must be made.

The IN-CONTEXT and NO-DEFAULT switches are combined using the
following prefix arguments:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (let* ((arg (prefix-numeric-value current-prefix-arg))
         (no-default
          (or no-default
              ;; The `completing-read' function provided by XEmacs
              ;; (21.1) don't allow a default value argument :-(
              (featurep 'xemacs)
              (= arg -1)                ; C-u -
              (= arg 16)))              ; C-u C-u
         (in-context
          (or in-context
              (= arg 4)                 ; C-u
              (= arg 16)))              ; C-u C-u
         (context
          (and (not no-default)
               (or (semantic-ctxt-current-symbol)
                   (semantic-ctxt-current-function))))
         (completing-read-args
          (list (if (and context (car context))
                    (format "%s(default: %s) " prompt (car context))
                  prompt)
                (setq senator-jump-completion-list
                      (senator-completion-list in-context))
                nil
                require-match
                ""
                'semantic-read-symbol-history)))
    (list
     (apply #'completing-read
            (if (and context (car context))
                (append completing-read-args context)
              completing-read-args))
     in-context no-default)))

(defun senator-jump-noselect (sym &optional next-p regexp-p)
  "Jump to the semantic symbol SYM.
If NEXT-P is non-nil, then move the the next tag in the search
assuming there was already one jump for the given symbol.
If REGEXP-P is non nil, then treat SYM as a regular expression.
Return the tag jumped to.
Note: REGEXP-P doesn't work yet.  This needs to be added to get
the etags override to be fully functional."
  (let ((tag (cdr (assoc sym senator-jump-completion-list))))
    (when tag
      (set-buffer (semantic-tag-buffer tag))
      (goto-char (semantic-tag-start tag))
      tag)))

;;;###autoload
(defun senator-jump (sym &optional in-context no-default)
  "Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: " nil nil t))
  (push-mark)
  (let ((tag (senator-jump-noselect sym no-default)))
    (when tag
      (switch-to-buffer (semantic-tag-buffer tag))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s "
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)))))

;;;###autoload
(defun senator-jump-regexp (symregex &optional in-context no-default)
  "Jump to the semantic symbol SYMREGEX.
SYMREGEX is treated as a regular expression.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value and move to the
next match of SYMREGEX.  NOTE: Doesn't actually work yet.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: "))
  (let ((tag (senator-jump-noselect symregex no-default)))
    (when tag
      (switch-to-buffer (semantic-tag-buffer tag))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s "
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)))))

(defvar senator-last-completion-stats nil
  "The last senator completion was here.
Of the form (BUFFER STARTPOS INDEX REGEX COMPLIST...)")

(defsubst senator-current-symbol-start ()
  "Return position of start of the current symbol under point or nil."
  (let* ((sb (semantic-ctxt-current-symbol-and-bounds (point)))
         (bounds (nth 2 sb)))
    (car bounds)))

;;  (condition-case nil
;;      (save-excursion (forward-sexp -1) (point))
;;    (error nil)))

;;;###autoload
(defun senator-complete-symbol (&optional cycle-once)
  "Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((symstart (senator-current-symbol-start))
        regex complst)
    (if symstart
        ;; Get old stats if apropriate.
        (if (and senator-last-completion-stats
                 ;; Check if completing in the same buffer
                 (eq (car senator-last-completion-stats) (current-buffer))
                 ;; Check if completing from the same point
                 (= (nth 1 senator-last-completion-stats) symstart)
                 ;; Check if completing the same symbol
                 (save-excursion
                   (goto-char symstart)
                   (looking-at (nth 3 senator-last-completion-stats))))

            (setq complst (nthcdr 4 senator-last-completion-stats))

          (setq regex (regexp-quote (buffer-substring symstart (point)))
                complst (senator-find-tag-for-completion regex)
                senator-last-completion-stats (append (list (current-buffer)
                                                            symstart
                                                            0
                                                            regex)
                                                      complst))))
    ;; Do the completion if appropriate.
    (if complst
        (let ((ret   t)
              (index (nth 2 senator-last-completion-stats))
              newtok)
          (if (= index (length complst))
              ;; Cycle to the first completion tag.
              (setq index  0
                    ;; Stop completion if CYCLE-ONCE is non-nil.
                    ret (not cycle-once)))
          ;; Get the new completion tag.
          (setq newtok (nth index complst))
          (when ret
            ;; Move index to the next completion tag.
            (setq index (1+ index)
                  ;; Return the completion string (useful to hippie
                  ;; expand for example)
                  ret   (semantic-tag-name newtok))
            ;; Replace the string.
            (delete-region symstart (point))
            (insert ret))
          ;; Update the completion index.
          (setcar (nthcdr 2 senator-last-completion-stats) index)
          ret))))

;;;;
;;;; Completion menu
;;;;

(defcustom senator-completion-menu-summary-function
  'semantic-format-tag-concise-prototype
  "*Function to use when creating items in completion menu.
Some useful functions are in `semantic-format-tag-functions'."
  :group 'senator
  :type semantic-format-tag-custom-list)
(make-variable-buffer-local 'senator-completion-menu-summary-function)

(defcustom senator-completion-menu-insert-function
  'senator-completion-menu-insert-default
  "*Function to use to insert an item from completion menu.
It will receive a Semantic tag as argument."
  :group 'senator
  :type '(radio (const senator-completion-menu-insert-default)
                (function)))
(make-variable-buffer-local 'senator-completion-menu-insert-function)

(defun senator-completion-menu-insert-default (tag)
  "Insert a text representation of TAG at point."
  (insert (semantic-tag-name tag)))

(defun senator-completion-menu-do-complete (tag-array)
  "Replace the current syntactic expression with a chosen completion.
Argument TAG-ARRAY is an array of one element containting the tag
choosen from the completion menu."
  (let ((tag (aref tag-array 0))
        (symstart (senator-current-symbol-start))
        (finsert (if (fboundp senator-completion-menu-insert-function)
                     senator-completion-menu-insert-function
                   #'senator-completion-menu-insert-default)))
    (if symstart
        (progn
          (delete-region symstart (point))
          (funcall finsert tag)))))

(defun senator-completion-menu-item (tag)
  "Return a completion menu item from TAG.
That is a pair (MENU-ITEM-TEXT . TAG-ARRAY).  TAG-ARRAY is an
array of one element containing TAG.  Can return nil to discard a
menu item."
  (cons (funcall (if (fboundp senator-completion-menu-summary-function)
                     senator-completion-menu-summary-function
                   #'semantic-format-tag-prototype) tag)
        (vector tag)))

(defun senator-completion-menu-window-offsets (&optional window)
  "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET)) in
WINDOW'S frame."
  (let* ((window  (or window (selected-window)))
         (e       (window-edges window))
         (left    (nth 0 e))
         (top     (nth 1 e))
         (right   (nth 2 e))
         (bottom  (nth 3 e))
         (x       (+ left (/ (- right left) 2)))
         (y       (+ top  (/ (- bottom top) 2)))
         (wpos    (coordinates-in-window-p (cons x y) window))
         (xoffset 0)
         (yoffset 0))
    (if (consp wpos)
        (let* ((f  (window-frame window))
               (cy (/ 1.0 (float (frame-char-height f)))))
          (setq xoffset (- x (car wpos))
                yoffset (float (- y (cdr wpos))))
          ;; If Emacs 21 add to:
          ;; - XOFFSET the WINDOW left margin width.
          ;; - YOFFSET the height of header lines above WINDOW.
          (if (> emacs-major-version 20)
              (progn
                (setq wpos    (cons (+ left xoffset) 0.0)
                      bottom  (float bottom))
                (while (< (cdr wpos) bottom)
                  (if (eq (coordinates-in-window-p wpos window)
                          'header-line)
                      (setq yoffset (+ yoffset cy)))
                  (setcdr wpos (+ (cdr wpos) cy)))
                (setq xoffset (floor (+ xoffset
                                        (or (car (window-margins window))
                                            0))))))
          (setq yoffset (floor yoffset))))
    (cons xoffset yoffset)))

(defun senator-completion-menu-point-as-event()
  "Returns the text cursor position as an event.
Also move the mouse pointer to the cursor position."
  (let* ((w (get-buffer-window (current-buffer)))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
         )
    (if (featurep 'xemacs)
        (let* ((at (progn (set-mouse-position w x (1- y))
                          (cdr (mouse-pixel-position))))
               (x  (car at))
               (y  (cdr at)))
          (make-event 'button-press
                      (list 'button 3
                            'modifiers nil
                            'x x
                            'y y)))
      ;; Emacs
      (let ((offsets (senator-completion-menu-window-offsets w)))
        ;; Convert window position (x,y) to the equivalent frame
        ;; position and move the mouse pointer to it.
        (set-mouse-position (window-frame w)
                            (+ x (car offsets))
                            (+ y (cdr offsets)))
        t))))

;;;###autoload
(defun senator-completion-menu-popup ()
  "Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((symstart (senator-current-symbol-start))
        symbol regexp complst
        ;; Turn off tag jumping for this menu.
        (imenu-default-goto-function (lambda (name pos &optional rest) pos)))
    (if symstart
        (setq symbol  (buffer-substring-no-properties symstart (point))
              regexp  (regexp-quote symbol)
              complst (senator-find-tag-for-completion regexp)))
    (if (not (car complst))
        (error "No completions available")
      ;; We have a completion list, build a menu
      (let ((index (delq nil
			 (mapcar #'senator-completion-menu-item
				 complst)))
	    title item)
	(cond ;; Here index is a menu structure like:

	 ;; -1- (("menu-item1" . [tag1]) ...)
	 ((vectorp (cdr (car index)))
	  ;; There are more than one item, setup the popup title.
	  (if (cdr index)
	      (setq title (format "%S completion" symbol))
	    ;; Only one item , no need to popup the menu.
	    (setq item (car index))))

	 ;; -2- (("menu-title1" ("menu-item1" . [tag1]) ...) ...)
	 (t
	  ;; There are sub-menus.
	  (if (cdr index)
	      ;; Several sub-menus, setup the popup title.
	      (setq title (format "%S completion" symbol))
	    ;; Only one sub-menu, convert it to a main menu and add the
	    ;; sub-menu title (filename) to the popup title.
	    (setq title (format "%S completion (%s)"
				symbol (car (car index)))
		  index (cdr (car index)))
	    ;; But...
	    (or (cdr index)
		;; ... If only one menu item, no need to popup the menu.
		(setq item (car index))))))
	(or item
	    ;; `imenu--mouse-menu' automagically splits large menu into
	    ;; several submenus, displays the popup menu, and returns
	    ;; the selected item :-)
	    (setq item (imenu--mouse-menu
			index
			;; popup at point
			(senator-completion-menu-point-as-event)
			title)))
	(if item
	    (senator-completion-menu-do-complete (cdr item)))))))

;;;;
;;;; Search commands
;;;;

;;;###autoload
(defun senator-search-forward (string &optional bound noerror count)
  "Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic search: ")
  (senator-search 'search-forward string bound noerror count))

(defun senator-re-search-forward (regexp &optional bound noerror count)
  "Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic regexp search: ")
  (senator-search 're-search-forward regexp bound noerror count))

(defun senator-word-search-forward (word &optional bound noerror count)
  "Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic word search: ")
  (senator-search 'word-search-forward word bound noerror count))

(defun senator-search-backward (string &optional bound noerror count)
  "Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward search: ")
  (senator-search 'search-backward string bound noerror count))

(defun senator-re-search-backward (regexp &optional bound noerror count)
  "Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward regexp search: ")
  (senator-search 're-search-backward regexp bound noerror count))

(defun senator-word-search-backward (word &optional bound noerror count)
  "Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward word search: ")
  (senator-search 'word-search-backward word bound noerror count))

;;; Other useful search commands (minor mode menu)

(defvar senator-last-search-type nil
  "Type of last non-incremental search command called.")

(defun senator-nonincremental-repeat-search-forward ()
  "Search forward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-forward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-forward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-repeat-search-backward ()
  "Search backward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-backward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-backward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-search-forward (string)
  "Search for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-forward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-forward string)))

(defun senator-nonincremental-search-backward (string)
  "Search backward for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-backward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-backward string)))

(defun senator-nonincremental-re-search-forward (string)
  "Search for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-forward string)))

(defun senator-nonincremental-re-search-backward (string)
  "Search backward for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-backward string)))

(defvar senator--search-filter nil)

(defun senator-search-set-tag-class-filter (&optional classes)
  "In current buffer, limit search scope to tag CLASSES.
CLASSES is a list of tag class symbols or nil.  If nil only global
filters in `senator-search-tag-filter-functions' remain active."
  (interactive "sClasses: ")
  (setq classes
        (cond
         ((null classes)
          nil)
         ((symbolp classes)
          (list classes))
         ((stringp classes)
          (mapcar 'read (split-string classes)))
         (t
          (signal 'wrong-type-argument (list classes)))
         ))
  ;; Clear previous filter.
  (remove-hook 'senator-search-tag-filter-functions
               senator--search-filter t)
  (kill-local-variable 'senator--search-filter)
  (if classes
      (let ((tag   (make-symbol "tag"))
            (names (mapconcat 'symbol-name classes "', `")))
        (set (make-local-variable 'senator--search-filter)
             `(lambda (,tag)
                (memq (semantic-tag-class ,tag) ',classes)))
        (add-hook 'senator-search-tag-filter-functions
                  senator--search-filter nil t)
        (message "Limit search to `%s' tags" names))
    (message "Default search filter restored")))

;;; Folding
;;
;; Use new folding state.  It might be wise to extend the idea
;; of folding for hiding all but this, or show all children, etc.

(defun senator-fold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) t))

(defun senator-unfold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) nil))

(defun senator-fold-tag-toggle (&optional tag)
  "Fold the current TAG."
  (interactive)
  (let ((tag (or tag (semantic-current-tag))))
    (if (semantic-tag-folded-p tag)
        (senator-unfold-tag tag)
      (senator-fold-tag tag))))

;; @TODO - move this to some analyzer / refs tool
(define-overloadable-function semantic-up-reference (tag)
  "Return a tag that is referred to by TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features.")

;;;###autoload
(defun senator-go-to-up-reference (&optional tag)
  "Move up one reference from the current TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((result (semantic-up-reference (or tag (semantic-current-tag)))))
    (if (not result)
        (error "No up reference found")
      (push-mark)
      (cond
       ;; A tag
       ((semantic-tag-p result)
	(semantic-go-to-tag result)
	(switch-to-buffer (current-buffer))
	(semantic-momentary-highlight-tag result))
       ;; Buffers
       ((bufferp result)
	(switch-to-buffer result)
	(pulse-momentary-highlight-one-line (point)))
       ;; Files
       ((and (stringp result) (file-exists-p result))
	(find-file result)
	(pulse-momentary-highlight-one-line (point)))
       (t
	(error "Unknown result type from `semantic-up-reference'"))))))

(defun semantic-up-reference-default (tag)
  "Return a tag that is referred to by TAG.
Makes C/C++ language like assumptions."
  (cond ((semantic-tag-faux-p tag)
         ;; Faux tags should have a real tag in some other location.
	 (require 'semantic/sort)
         (let ((options (semantic-tag-external-class tag)))
           ;; I should do something a little better than
           ;; this.  Oy!
           (car options)
           ))

	;; Include always point to another file.
        ((eq (semantic-tag-class tag) 'include)
	 (let ((file (semantic-dependency-tag-file tag)))
	   (cond
	    ((or (not file) (not (file-exists-p file)))
	     (error "Could not location include %s"
		    (semantic-tag-name tag)))
	    ((get-file-buffer file)
	     (get-file-buffer file))
	    ((stringp file)
	     file)
	    )))

	;; Is there a parent of the function to jump to?
        ((and (semantic-tag-of-class-p tag 'function)
              (semantic-tag-function-parent tag))
         (let* ((scope (semantic-calculate-scope (point))))
	   ;; @todo - it would be cool to ask the user which one if
	   ;; more than one.
	   (car (oref scope parents))
	   ))

	;; Is there a non-prototype version of the tag to jump to?
        ((semantic-tag-get-attribute tag :prototype-flag)
	 (require 'semantic/analyze/refs)
	 (let* ((sar (semantic-analyze-tag-references tag)))
	   (car (semantic-analyze-refs-impl sar t)))
	 )

	;; If this is a datatype, and we have superclasses
	((and (semantic-tag-of-class-p tag 'type)
	      (semantic-tag-type-superclasses tag))
	 (require 'semantic/analyze)
	 (let ((scope (semantic-calculate-scope (point)))
	       (parents (semantic-tag-type-superclasses tag)))
	   (semantic-analyze-find-tag (car parents) 'type scope)))

	;; Get the data type, and try to find that.
        ((semantic-tag-type tag)
	 (require 'semantic/analyze)
	 (let ((scope (semantic-calculate-scope (point))))
	   (semantic-analyze-tag-type tag scope))
	 )
        (t nil)))

(defvar senator-isearch-semantic-mode nil
  "Non-nil if isearch does semantic search.
This is a buffer local variable.")
(make-variable-buffer-local 'senator-isearch-semantic-mode)

(defun senator-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (semantic-fetch-tags)
  (let* ((senator-highlight-found nil)
         ;; Step at beginning of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-previous-tag)))
    (when tag
      (if (= (point) (semantic-tag-end tag))
          (goto-char (semantic-tag-start tag)))
      (beginning-of-line))))

(defun senator-end-of-defun (&optional arg)
  "Move forward to next end of defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (semantic-fetch-tags)
  (let* ((senator-highlight-found nil)
         ;; Step at end of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-next-tag)))
    (when tag
      (if (= (point) (semantic-tag-start tag))
          (goto-char (semantic-tag-end tag)))
      (skip-chars-forward " \t")
      (if (looking-at "\\s<\\|\n")
          (forward-line 1)))))

(defun senator-narrow-to-defun ()
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (semantic-fetch-tags)
  (save-excursion
    (widen)
    (senator-end-of-defun)
    (let ((end (point)))
      (senator-beginning-of-defun)
      (narrow-to-region (point) end))))

(defun senator-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (let ((origin (point))
        (end    (progn (senator-end-of-defun) (point)))
        (start  (progn (senator-beginning-of-defun) (point))))
    (goto-char origin)
    (push-mark (point))
    (goto-char end) ;; end-of-defun
    (push-mark (point) nil t)
    (goto-char start) ;; beginning-of-defun
    (re-search-backward "^\n" (- (point) 1) t)))

(defadvice beginning-of-defun (around senator activate)
  "Move backward to the beginning of a defun.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (called-interactively-p 'any))
      (senator-beginning-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-defun (around senator activate)
  "Move forward to next end of defun.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (called-interactively-p 'any))
      (senator-end-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice narrow-to-defun (around senator activate)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (called-interactively-p 'any))
      (senator-narrow-to-defun)
    ad-do-it))

(defadvice mark-defun (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (called-interactively-p 'any))
      (senator-mark-defun)
    ad-do-it))

(defadvice c-mark-function (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (called-interactively-p 'any))
      (senator-mark-defun)
    ad-do-it))

(defvar senator-add-log-tags '(function variable type)
  "When advising `add-log-current-defun', tag classes used.
Semantic tags that are of these classes will be used to find the name
used by add log.")
(semantic-varalias-obsolete 'senator-add-log-tokens
                            'senator-add-log-tags nil)

(defadvice add-log-current-defun (around senator activate)
  "Return name of function definition point is in, or nil."
  (if senator-minor-mode
      (let ((tag (semantic-current-tag))
            (name nil))
        (if (and tag (memq (semantic-tag-class tag)
                           senator-add-log-tags))
            (progn
              (setq name
		    (replace-regexp-in-string
		     "\\(?:(\\|)\\)" ""
		     (semantic-format-tag-canonical-name
		      tag (semantic-current-tag-parent))))
              (setq ad-return-value name))
          ad-do-it))
    ad-do-it))

;;;;
;;;; Tag Cut & Paste
;;;;

;; To copy a tag, means to put a tag definition into the tag
;; ring.  To kill a tag, put the tag into the tag ring AND put
;; the body of the tag into the kill-ring.
;;
;; To retrieve a killed tag's text, use C-y (yank), but to retrieve
;; the tag as a reference of some sort, use senator-yank-tag.

(defvar senator-tag-ring (make-ring 20)
  "Ring of tags for use with cut and paste.")

;;;###autoload
(defun senator-copy-tag ()
  "Take the current tag, and place it in the tag ring."
  (interactive)
  (semantic-fetch-tags)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (ring-insert senator-tag-ring ft)
      (kill-ring-save (semantic-tag-start ft) (semantic-tag-end ft))
      (when (called-interactively-p 'interactive)
        (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert.")))
    ft))

;;;###autoload
(defun senator-kill-tag ()
  "Take the current tag, place it in the tag ring, and kill it.
Killing the tag removes the text for that tag, and places it into
the kill ring.  Retrieve that text with \\[yank]."
  (interactive)
  (let ((ct (senator-copy-tag))) ;; this handles the reparse for us.
    (kill-region (semantic-tag-start ct)
                 (semantic-tag-end ct))
    (when (called-interactively-p 'interactive)
      (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert."))))

;;;###autoload
(defun senator-yank-tag ()
  "Yank a tag from the tag ring.
The form the tag takes is different depending on where it is being
yanked to."
  (interactive)
  (or (ring-empty-p senator-tag-ring)
      (let ((ft (ring-ref senator-tag-ring 0)))
          (semantic-foreign-tag-check ft)
          (semantic-insert-foreign-tag ft)
          (when (called-interactively-p 'interactive)
            (message "Use C-y to recover the yank the text of %s."
                     (semantic-tag-name ft))))))

;;;###autoload
(defun senator-copy-tag-to-register (register &optional kill-flag)
  "Copy the current tag into REGISTER.
Optional argument KILL-FLAG will delete the text of the tag to the
kill ring."
  (interactive "cTag to register: \nP")
  (semantic-fetch-tags)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (set-register register ft)
      (if kill-flag
          (kill-region (semantic-tag-start ft)
                       (semantic-tag-end ft))))))

;;;###autoload
(defun senator-transpose-tags-up ()
  "Transpose the current tag, and the preceding tag."
  (interactive)
  (semantic-fetch-tags)
  (let* ((current-tag (semantic-current-tag))
         (prev-tag (save-excursion
                     (goto-char (semantic-tag-start current-tag))
                     (semantic-find-tag-by-overlay-prev)))
         (ct-parent (semantic-find-tag-parent-by-overlay current-tag))
         (pt-parent (semantic-find-tag-parent-by-overlay prev-tag)))
    (if (not (eq ct-parent pt-parent))
        (error "Cannot transpose tags"))
    (let ((txt (buffer-substring (semantic-tag-start current-tag)
                                 (semantic-tag-end current-tag)))
          (line (count-lines (semantic-tag-start current-tag)
                             (point)))
          (insert-point nil)
          )
      (delete-region (semantic-tag-start current-tag)
                     (semantic-tag-end current-tag))
      (delete-blank-lines)
      (goto-char (semantic-tag-start prev-tag))
      (setq insert-point (point))
      (insert txt)
      (if (/= (current-column) 0)
          (insert "\n"))
      (insert "\n")
      (goto-char insert-point)
      (forward-line line)
      )))

;;;###autoload
(defun senator-transpose-tags-down ()
  "Transpose the current tag, and the following tag."
  (interactive)
  (semantic-fetch-tags)
  (let* ((current-tag (semantic-current-tag))
         (next-tag (save-excursion
                     (goto-char (semantic-tag-end current-tag))
                     (semantic-find-tag-by-overlay-next)))
         (end-pt (point-marker))
         )
    (goto-char (semantic-tag-start next-tag))
    (forward-char 1)
    (senator-transpose-tags-up)
    ;; I know that the above fcn deletes the next tag, so our pt marker
    ;; will be stable.
    (goto-char end-pt)))

;;; Using semantic search in isearch mode

(defun senator-lazy-highlight-update ()
  "Force lazy highlight update."
  (lazy-highlight-cleanup t)
  (set 'isearch-lazy-highlight-last-string nil)
  (setq isearch-adjusted t)
  (isearch-update))

;; Recent versions of GNU Emacs allow to override the isearch search
;; function for special needs, and avoid to advice the built-in search
;; function :-)
(defun senator-isearch-search-fun ()
  "Return the function to use for the search.
Use a senator search function when semantic isearch mode is enabled."
  (intern
   (concat (if senator-isearch-semantic-mode
               "senator-"
             "")
           (cond (isearch-word "word-")
                 (isearch-regexp "re-")
                 (t ""))
           "search-"
           (if isearch-forward
               "forward"
             "backward"))))

(defun senator-isearch-toggle-semantic-mode ()
  "Toggle semantic searching on or off in isearch mode."
  (interactive)
  (setq senator-isearch-semantic-mode
	(not senator-isearch-semantic-mode))
  (if isearch-mode
      ;; force lazy highlight update
      (senator-lazy-highlight-update)
    (message "Isearch semantic mode %s"
	     (if senator-isearch-semantic-mode
		 "enabled"
	       "disabled"))))

(defvar senator-old-isearch-search-fun nil
  "Hold previous value of `isearch-search-fun-function'.")

(defun senator-isearch-mode-hook ()
  "Isearch mode hook to setup semantic searching."
  (if (and isearch-mode senator-isearch-semantic-mode)
      (progn
	;; When `senator-isearch-semantic-mode' is on save the
	;; previous `isearch-search-fun-function' and install the
	;; senator one.
	(when (and (local-variable-p 'isearch-search-fun-function)
		   (not (local-variable-p 'senator-old-isearch-search-fun)))
	  (set (make-local-variable 'senator-old-isearch-search-fun)
	       isearch-search-fun-function))
	(set (make-local-variable 'isearch-search-fun-function)
	     'senator-isearch-search-fun))
    ;; When `senator-isearch-semantic-mode' is off restore the
    ;; previous `isearch-search-fun-function'.
    (when (eq isearch-search-fun-function 'senator-isearch-search-fun)
      (if (local-variable-p 'senator-old-isearch-search-fun)
	  (progn
	    (set (make-local-variable 'isearch-search-fun-function)
		 senator-old-isearch-search-fun)
	    (kill-local-variable 'senator-old-isearch-search-fun))
	(kill-local-variable 'isearch-search-fun-function)))))

;; (add-hook 'isearch-mode-hook     'senator-isearch-mode-hook)
;; (add-hook 'isearch-mode-end-hook 'senator-isearch-mode-hook)

;; ;; Keyboard shortcut to toggle semantic search in isearch mode.
;; (define-key isearch-mode-map
;;   [(control ?,)]
;;   'senator-isearch-toggle-semantic-mode)

(provide 'semantic/senator)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/senator"
;; End:

;;; semantic/senator.el ends here
