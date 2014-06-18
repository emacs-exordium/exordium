;;; ecb-symboldef.el --- ECB window for symbol-definition or documentation

;;; Copyright (C) 2005 Hauke Jans

;; Author: Hauke Jans, <hauke.jans@sesa.de>, <hauke.jans@t-online.de>
;; Maintainer: Hauke Jans, <hauke.jans@sesa.de>, <hauke.jans@t-online.de>
;;             Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, symbol-definition
;; Created: 2005

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$

;;; Commentary:
;;
;; Define an ecb-buffer which shows in a special ecb window either the
;; documentation or the context of the definition of the current symbol
;; under point.
;;

;;; Usage
;;
;; Either use the layout "left-symboldef" (e.g. via [C-c . l c]) or create a
;; new ecb-layout via the command `ecb-create-new-layout' and add a buffer of
;; type "symboldef" into this new layout.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:


(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-common-browser)
(require 'ecb-cedet-wrapper)

(eval-when-compile
  (require 'silentcomp))

;; XEmacs-stuff
(silentcomp-defun find-tag-internal)
;; Emacs stuff
(silentcomp-defun find-tag-noselect)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: BTW add symboldef to the
;; maximize menu of ECB when reworked ecb-symboldef.el! and also to
;; some other menues...


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: add to ecb-modeline the W-1 stuff
;; for the symboldef-modeline.

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; 1. Add all necessary documentation to the info-manual (texi)
;; 2. Add this preferences group to the menu in ecb.el

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; new option which allows to exclude arbitrary fsymbols from being
;; docu-displayed.

(defgroup ecb-symboldef nil
  "Settings for the symbol-definition-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-symboldef-buffer-name " *ECB Symboldefinition*"
  "*Name of the ECB-symbol-definition buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Symboldefinition*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object
of the ECB-symbol-definition-buffer by this name, e.g. by a call of
`set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-symboldef
  :type 'string)

(defcustom ecb-symboldef-find-functions
  '((lisp-interaction-mode . ecb-symboldef-find-lisp-doc)
    (lisp-mode . ecb-symboldef-find-lisp-doc)
    (emacs-lisp-mode . ecb-symboldef-find-lisp-doc)
    (default . ecb-symboldef-find-definition))
  "*Funtions to find the definition or docu for symbol under point.
This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The car is a function intended to find the definition of a
  certain symbol for files of this major-mode. Such a function
  will be called with two arguments, the first is the symbol-name
  as string for which the definition or documentation should be
  displayed and the second the current edit-buffer as
  buffer-object, i.e. the current buffer of the current
  edit-window. The function will be called with the special
  ecb-symbol-definition-buffer as current buffer whereas this
  buffer is empty. The function has to insert everything
  necessary to display the symbol-definition or -documentation
  and is also responsible to format the displayed text. The
  buffer-local variable `fill-column is already preset to the
  window-width of the special ecb-window minus 1. The function is
  responsible to set the buffer-local variable `truncate-lines'
  appropriate. The function can either return nil or a \(small) string
  which will be integrated in the modeline-display of this
  ecb-window.

There are two predefined functions `ecb-symboldef-find-lisp-doc' and
`ecb-symboldef-find-definition' whereas the latter one is used as a default
find-function."
  :group 'ecb-symboldef
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (function :tag "Find function"))))

(defcustom ecb-symboldef-minimum-symbol-length 3
  "*Minimum length a symbol must have so its definition or doc is displayed."
  :group 'ecb-symboldef
  :type 'integer)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: This option is an example how the
;; user could determine which backends should be used for finding a definition
;; and also in which order the backends should be tried...
;; probbaly not necessary anymore with current cedet
;; (defcustom ecb-symboldef-find-backends '(semanticdb etags)
;;   "*Feature currently not implemented!"
;;   :group 'ecb-symboldef
;;   :type '(repeat (choice :tag "Backends"
;;                          :menu-tag "Backends"
;;                          (const :tag "semanticdb" :value semanticdb)
;;                          (const :tag "etags" :value etags)
;;                          (symbol :tag "Other"))))

(defcustom ecb-symboldef-buffer-sync 'basic
  "*Synchronize the symboldef buffer automatically with current edit buffer.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync'.

IMPORTANT NOTE: Every time the synchronization is done the hook
`ecb-symboldef-buffer-sync-hook' is evaluated."
  :group 'ecb-symboldef
  :type '(radio :tag "Synchronize ECBs symboldef buffer"
                (const :tag "Use basic value" :value basic)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))
    

(defcustom ecb-symboldef-buffer-sync-delay 'basic
  "*Time Emacs must be idle before the symboldef-buffer is synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay.

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync-delay'."
  :group 'ecb-symboldef
  :type '(radio (const :tag "Use basic value" :value basic)
                (const :tag "No synchronizing delay" :value nil)
                (number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-activate-ecb-autocontrol-function
                        value 'ecb-analyse-buffer-sync))))
  :initialize 'custom-initialize-default)
  
(defcustom ecb-symboldef-buffer-sync-hook nil
  "Hook run at the end of the function `ecb-symboldef-buffer-sync'.
See documentation of `ecb-symboldef-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Preconditions for such a hook:
- Current buffer is the buffer of the currently selected
  edit-window.
- The symboldef-buffer is displayed in a visible window of the
  ecb-frame \(so no check for visibilty of the symboldef-buffer in
  the ecb-frame is necessary in a hook function)

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If the option `ecb-symboldef-buffer-sync' is not
nil the function `ecb-symboldef-buffer-sync' is running either
every time Emacs is idle or even after every command \(see
`ecb-symboldef-buffer-sync-delay'). So if the symboldef-buffer is
displayed in a window of the ecb-frame \(see preconditions above)
these hooks can be really called very often! Therefore each
function of this hook should/must check in an efficient way at
beginning if its task have to be really performed and then do
them only if really necessary! Otherwise performance of Emacs
could slow down dramatically!"
  :group 'ecb-symboldef
  :type 'hook)


;; ---- internal variables -----------

(defvar ecb-symboldef-last-symbol nil
  "Holds the previous symbol under cursor")

(defun ecb-symboldef-get-find-function ()
  "Returns the symbol find function to use according to major-mode"
  (let ((mode-function (cdr (assoc major-mode ecb-symboldef-find-functions)))
	(default-function (cdr (assoc 'default ecb-symboldef-find-functions))))
    (or mode-function
        default-function
        'ecb-symboldef-find-null)))

(defun ecb-symboldef-find-null (symbol-name edit-buffer)
  "Empty symbol-definition find function. 
Only prints mode and info but does not find any symbol-definition."
  (let ((symboldef-window-height (ecb-window-full-height
                                  (get-buffer-window (current-buffer)))))
    (dotimes (i (/ symboldef-window-height 2)) (insert "\n"))
    (insert  "*  No symbol definition function for current mode *\n"
             "*  See variable `ecb-symboldef-find-functions' *")))

;; --------------- code for finding the elisp docu -----------------------------

(defconst ecb-symboldef-temp-buffer-name " *ecb-symboldef-temp-buffer")

;; currently not used 
(defun ecb-symboldef-get-doc-for-fsymbol (fsymbol edit-buffer)
  "Returns the full output of `describe-function' as string without any
sideeffect to the help-system of Emacs.
FSYMBOL is the symbol for which the doc-string should be returned and
EDIT-BUFFER is that buffer FSYMBOL is used."
  ;; by binding standard-output to a special buffer we can force
  ;; describe-function-1 to print all its output to this buffer. 
  (let ((standard-output (get-buffer-create ecb-symboldef-temp-buffer-name))
        (doc-string nil))
    (with-current-buffer standard-output
      ;;(insert (symbol-name symbol))
      (describe-function-1 fsymbol)
      (setq doc-string (buffer-string)))
    (kill-buffer standard-output)
    doc-string))


(defun ecb-symboldef-get-doc-for-vsymbol (vsymbol edit-buffer)
  "Returns the full output of `describe-variable' as string without any
sideeffect to the help-system of Emacs.
VSYMBOL is the symbol for which the doc-string should be returned and
EDIT-BUFFER is that buffer VSYMBOL is used."
  ;; Klaus Berndl <klaus.berndl@sdm.de>: It would be good if we could
  ;; use the describe-variable output. Problem: This function displays the
  ;; doc in a help-window, so we have to redirect the output. Possible but
  ;; there is something to do:
  ;; 0. making edit-buffer (s. arg) current
  ;; 1. For Emacs: redefining help-buffer so it returns a buffer which is
  ;;    suitable for us. For XEmacs help-buffer-name analogous
  ;; 2. For (X)Emacs: Setting temp-buffer-show-function temporally to
  ;;    something special which does simply nothing
  ;; 3. setting temp-buffer-setup-hook to nil
  ;; 3. Binding standart-output to a temporally buffer-object
  ;; 4. running describe-variable
  (with-current-buffer edit-buffer
    (let ((standard-output (get-buffer-create ecb-symboldef-temp-buffer-name))
          (temp-buffer-setup-hook nil)
          ;; this does not call temp-buffer-show-hook!!!
          (temp-buffer-show-function (function (lambda (b) nil))))
      (cl-flet ((help-buffer () ecb-symboldef-temp-buffer-name)
             ;; (print-help-return-message (&optional function) nil)
             ;; for XEmacs
             (help-buffer-name () ecb-symboldef-temp-buffer-name))
        (cl-labels ((print-help-return-message (&optional function) nil))
          (describe-variable vsymbol)))))
  (with-current-buffer ecb-symboldef-temp-buffer-name
    (when (member 'eieio-help-mode-augmentation-maybee temp-buffer-show-hook)
      (let ((major-mode 'help-mode))
        (eieio-help-mode-augmentation-maybee)))
    (buffer-string)))

(silentcomp-defun function-at-point)
(silentcomp-defun function-called-at-point)

(defun ecb-function-at-point ()
  "Return the function whose name is around point.
If that gives no function, return the function which is called by the
list containing point.  If that doesn't give a function, return nil."
  (or (ignore-errors
	(with-syntax-table emacs-lisp-mode-syntax-table
	  (save-excursion
	    (or (not (zerop (skip-syntax-backward "_w")))
		(eq (char-syntax (char-after (point))) ?w)
		(eq (char-syntax (char-after (point))) ?_)
		(forward-sexp -1))
	    (skip-chars-forward "`'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) (fboundp obj) obj)))))
      (ignore-errors
	(save-excursion
	  (save-restriction
	    (narrow-to-region (max (point-min) (- (point) 1000))
			      (point-max))
	    (backward-up-list 1)
	    (forward-char 1)
	    (let (obj)
	      (setq obj (read (current-buffer)))
	      (and (symbolp obj) (fboundp obj) obj)))))))

(defun ecb-symboldef-function-at-point ()
  "Returns the function around point or nil if there is no function around."
  (if ecb-running-xemacs
      (function-at-point)
    (function-called-at-point)))
    
(defun ecb-symboldef-find-lisp-doc (symbol-name edit-buffer)
  "Insert the lisp-documentation of symbol with name SYMBOL-NAME."
  (setq truncate-lines nil)
  (let ((symbol (intern symbol-name))
        (fsymbol nil)
        (begin-vdoc (point-min))
        (retval nil)
        (args nil))
    ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: make an option how to deal
    ;; with function-symbols:
    ;; - always display only if symbol under point is a function
    ;; - display function around
    ;; - display conditions for function symbols (length, regexp to match)
    (when (setq fsymbol
                (with-current-buffer edit-buffer
                  (ecb-symboldef-function-at-point)))
      (unless ecb-running-xemacs
        ;; With XEmacs the symbol itself is already contained in the
        ;; docstring describe-function-1 returns - with Emacs we must add it
        ;; for ourself.
        (insert (format "%s is " fsymbol)))
      (let ((standard-output (current-buffer)))
        (describe-function-1 fsymbol))
      (let ((beg nil)
            (end nil))
        (goto-char (point-min))
        (when (and ecb-symboldef-symbol-face
                   (re-search-forward (regexp-quote (symbol-name fsymbol)) nil t))
          (setq beg (match-beginning 0))
          (setq end (match-end 0))
          (ecb-merge-face
           (if (eq ecb-symboldef-symbol-face 'use-font-lock-face)
               'font-lock-function-name-face
             ecb-symboldef-symbol-face)
           beg end)
          (goto-char end))
        
        (when (and ecb-symboldef-prototype-face
                   (re-search-forward  (regexp-quote (concat "(" (symbol-name fsymbol))) nil t))
          (setq beg (match-beginning 0))
          (goto-char beg)
          (forward-sexp)
          (setq end (point))
          (ecb-merge-face
           ecb-symboldef-prototype-face
           beg end)
          (ecb-merge-face
           (if (eq ecb-symboldef-symbol-face 'use-font-lock-face)
               'font-lock-function-name-face
             ecb-symboldef-symbol-face)
           (1+ beg)
           (match-end 0))))
      (goto-char (point-max))
      ;; we needs this later if the symbol is not only a function but a
      ;; variable too!
      (setq begin-vdoc (point))
      (setq retval (format "Lisp %s"
                           (if (commandp fsymbol)
                               "Command"
                             "Function"))))
    (when (boundp symbol)
      (and fsymbol (insert "\n\n___________\n\n"))
;;       (insert (format "%s is a %s\n\n%s\n\nValue: %s\n\n" symbol
;;                       (if (user-variable-p symbol)
;;                           "Option " "Variable")
;;                       (or (documentation-property
;;                            symbol 'variable-documentation)
;;                           "not documented")
;;                       (symbol-value symbol)))
      (insert (ecb-symboldef-get-doc-for-vsymbol symbol edit-buffer))
      (let ((beg nil)
            (end nil))
        (goto-char begin-vdoc)
        (when (and ecb-symboldef-symbol-face
                   (re-search-forward (regexp-quote symbol-name) nil t))
          (setq beg (match-beginning 0))
          (setq end (match-end 0))
          (ecb-merge-face
           (if (eq ecb-symboldef-symbol-face 'use-font-lock-face)
               'font-lock-variable-name-face
             ecb-symboldef-symbol-face)
           beg end)
          (goto-char end)))
      (setq retval "Lisp Variable"))
    (goto-char (point-min))
    (fill-region (point-min) (point-max) 'left t)
    retval))

;; --------------- end of code for finding the elisp docu ----------------------


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: use the following mechanisms to
;; display the stuff - for elisp check if symbol is a tag of class 'type and
;; then display the parts below the docstring
;; (defun semantic-ia-show-doc (point)
;;   "Display the code-level documentation for the symbol at POINT."
;;   (interactive "d")
;;   (let* ((ctxt (semantic-analyze-current-context point))
;; 	 (pf (reverse (oref ctxt prefix)))
;; 	 )
;;     ;; If PF, the prefix is non-nil, then the last element is either
;;     ;; a string (incomplete type), or a semantic TAG.  If it is a TAG
;;     ;; then we should be able to find DOC for it.
;;     (cond 
;;      ((stringp (car pf))
;;       (message "Incomplete symbol name."))
;;      ((semantic-tag-p (car pf))
;;       ;; The `semantic-documentation-for-tag' fcn is language
;;       ;; specific.  If it doesn't return what you expect, you may
;;       ;; need to implement something for your language.
;;       ;;
;;       ;; The default tries to find a comment in front of the tag
;;       ;; and then strings off comment prefixes.
;;       (let ((doc (semantic-documentation-for-tag (car pf))))
;; 	(with-output-to-temp-buffer "*TAG DOCUMENTATION*"
;; 	  (princ "Tag: ")
;; 	  (princ (semantic-format-tag-prototype (car pf)))
;; 	  (princ "\n")
;; 	  (princ "\n")
;; 	  (princ "Snarfed Documentation: ")
;; 	  (princ "\n")
;; 	  (princ "\n")
;; 	  (if doc
;; 	      (princ doc)
;; 	    (princ "  Documentation unavailable."))
;; 	  )))
;;      (t
;;       (message "Unknown tag.")))
;;     ))

;; (defun semantic-ia-describe-class (typename)
;;   "Display all known parts for the datatype TYPENAME.
;; If the type in question is a class, all methods and other accessible
;; parts of the parent classes are displayed."
;;   ;; @todo - use a fancy completing reader.
;;   (interactive "sType Name: ")

;;   ;; When looking for a tag of any name there are a couple ways to do
;;   ;; it.  The simple `semanticdb-find-tag-by-...' are simple, and
;;   ;; you need to pass it the exact name you want.
;;   ;; 
;;   ;; The analyzer function `semantic-analyze-tag-name' will take
;;   ;; more complex names, such as the cpp symbol foo::bar::baz,
;;   ;; and break it up, and dive through the namespaces.
;;   (let ((class (semantic-analyze-find-tag typename)))

;;     (when (not (semantic-tag-p class))
;;       (error "Cannot find class %s" class))
;;     (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
;;       ;; There are many semantic-format-tag-* fcns.
;;       ;; The summarize routine is a fairly generic one.
;;       (princ (semantic-format-tag-summarize class))
;;       (princ "\n")
;;       (princ "  Type Members:\n")
;;       ;; The type tag contains all the parts of the type.
;;       ;; In complex languages with inheritance, not all the
;;       ;; parts are in the tag.  This analyzer fcn will traverse
;;       ;; the inheritance tree, and find all the pieces that
;;       ;; are inherited.
;;       (let ((parts (semantic-analyze-scoped-type-parts class)))
;; 	(while parts
;; 	  (princ "    ")
;; 	  (princ (semantic-format-tag-summarize (car parts)))
;; 	  (princ "\n")
;; 	  (setq parts (cdr parts)))
;; 	)
;;       )))


(defun ecb-symboldef-find-tag-by-semanticdb (symbol-name edit-buffer)
  "Function to find a semantic-tag by SYMBOL-NAME.
Returns nil if not found otherwise a list \(tag-buffer tag-begin tag-end)"
  (with-current-buffer edit-buffer
    (let* ((mytag-list (ecb--semanticdb-brute-deep-find-tags-by-name symbol-name
                                                                     nil t))
	   (mytag (if mytag-list 
                      (car (ecb--semanticdb-find-result-nth
                            mytag-list
                            (1- (ecb--semanticdb-find-result-length mytag-list))))))
	   (mytag-ovr (if mytag (ecb--semantic-tag-bounds mytag)))
	   (mytag-min (if mytag-ovr (car mytag-ovr)))
	   (mytag-max (if mytag-ovr (car (cdr mytag-ovr))))
	   (mytag-buf (if mytag (ecb--semantic-tag-buffer mytag))))
      (if mytag-buf
          (list mytag-buf mytag-min mytag-max)))))

(defun ecb-symboldef-find-tag-by-etags (symbol-name edit-buffer)
  "Try to find the definition of SYMBOL-NAME via etags.
Returns nil if not found otherwise a list \(tag-buffer tag-begin tag-end)
whereas tag-end is currently always nil."
  (if ecb-running-xemacs
      (let ((result (ignore-errors (find-tag-internal (list symbol-name)))))
	(if result
	    (list (car result) (cdr result) nil)))
    ;; else gnu emacs:
    (let* ((result-buf (ignore-errors (find-tag-noselect symbol-name)))
	   (result-point (if result-buf 
                             (with-current-buffer result-buf
                               (point)))))
      (if result-buf
	  (list result-buf result-point nil)))))

(defun ecb-symboldef-find-definition (symbol-name edit-buffer)
  "Inserts the definition of symbol with name SYMBOL-NAME.
Fill the upper-half of the special ecb-window with text preceding the
symbol-definition in the definition-file. First tries to find the definition
with semanticdb and then - if no success - with current etags-file."
  (let* ((symboldef-window-height (ecb-window-full-height
                                   (get-buffer-window (current-buffer))))
         ;; first lookup via semnaticdb, then via etags:
         (result (or (ecb-symboldef-find-tag-by-semanticdb symbol-name edit-buffer)
                     (ecb-symboldef-find-tag-by-etags symbol-name edit-buffer)
                     (list nil nil nil)))
         (num-tag-lines (- (/ symboldef-window-height 2) 0))
         (tag-buf (nth 0 result))
         (tag-point (nth 1 result))
         (tag-point-max (nth 2 result))
         (extend-point-min nil)
         (extend-point-max nil)
         (hilight-point-min nil)
         (hilight-point-max nil))
    (setq truncate-lines t)
    (when tag-buf
      (with-current-buffer tag-buf
        (save-excursion
          (goto-char tag-point)
          (forward-line (- num-tag-lines))
          (setq extend-point-min (point))
          (forward-line num-tag-lines)
          (forward-line num-tag-lines)
          (setq extend-point-max (point))))
      (insert (ecb-buffer-substring extend-point-min extend-point-max tag-buf))
      (goto-char (+ (- tag-point extend-point-min) 1))
      (setq hilight-point-min (point))
      (if tag-point-max 
          (goto-char (+ (- tag-point-max extend-point-min) 1))
        (end-of-line))
      (setq hilight-point-max (point))
      (add-text-properties hilight-point-min hilight-point-max
                           '(face highlight ))
      ;; return value
      (buffer-name tag-buf))))

(defecb-autocontrol/sync-function ecb-symboldef-buffer-sync
    ecb-symboldef-buffer-name ecb-symboldef-buffer-sync t
  "Synchronizes the symbol-definition buffer with current source if changed.
Can be called interactively but normally this should not be necessary because
it will be called by internal idle-mechanism'.

Runs the finder of `ecb-symboldef-find-functions' for current
symbol. Displays the found text in the buffer of
`ecb-symboldef-buffer-name' if it is displayed in a window of the ecb-frame."
  (save-excursion
    (let ((modeline-display nil)
          (edit-buffer (current-buffer))
          (current-symbol (ignore-errors (ecb-thing-at-point 'symbol)))
          ;; find tag search function according to mode:
          (find-func (ecb-symboldef-get-find-function)))
      ;; only use tags with a minimal length:
      (setq current-symbol (if (>= (length current-symbol)
                                   ecb-symboldef-minimum-symbol-length)
                               current-symbol))
      ;; buggy thingatpt returns whole buffer if on empty line:
      (setq current-symbol (if (< (length current-symbol) 80)
                               current-symbol))
      ;; research tag only if different from last and not empty: 
      (when (and current-symbol
                 (not (equal current-symbol ecb-symboldef-last-symbol)))
        (ecb-with-readonly-buffer visible-buffer
          (setq ecb-symboldef-last-symbol current-symbol)
          (erase-buffer)
          (setq fill-column (1- (window-width visible-window)))
          (setq modeline-display
                (or (funcall find-func
                             current-symbol
                             edit-buffer)
                    ""))
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: replace this by
          ;; a ecb-mode-line-format - if possible?!
          (ecb-mode-line-set (buffer-name visible-buffer)
                             (selected-frame)
                             (format "* Def %s <<%s>> *"
                                     modeline-display current-symbol)
                             nil t)
          ))))
  (run-hooks 'ecb-symboldef-buffer-sync-hook))


(defecb-window-dedicator-to-ecb-buffer ecb-set-symboldef-buffer
    ecb-symboldef-buffer-name nil
  "Set the buffer in the current window to the tag-definition-buffer and make
this window dedicated for this buffer."
  (switch-to-buffer (get-buffer-create ecb-symboldef-buffer-name))
  (ecb-activate-ecb-autocontrol-function ecb-symboldef-buffer-sync-delay
                                         'ecb-symboldef-buffer-sync))

(defun ecb-maximize-window-symboldef ()
  "Maximize the ECB-symbol-defnition window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-symboldefinition-window is not visible in current layout."
  (interactive)
  (ecb-maximize-ecb-buffer ecb-symboldef-buffer-name t))

(defun ecb-goto-window-symboldef ()
  "Make the ECB-symbol-definition window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-symboldef-buffer-name))

(silentcomp-provide 'ecb-symboldef)

;;; ecb-symboldef.el ends here
