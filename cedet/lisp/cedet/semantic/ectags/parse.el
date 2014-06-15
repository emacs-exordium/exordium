;;; semantic/ectags/parse.el --- exuberant CTags into Semantic tags

;; Copyright (C) 2008, 2009, 2010, 2011, 2012 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Converting CTags output tables into Semantic Tag Tables.
;;
;; This works by scanning the output of a CTags output buffer,
;; and instead creating semantic tags for each line.
;;
;; Tags that appear as members, or otherwise appear to belong to
;; other tags in the list will be parented appropriately.

(require 'mode-local)
(require 'semantic)
(require 'semantic/tag)
(require 'semantic/analyze)
(require 'data-debug)
(require 'semantic/ectags/util)

;;; Code:

;; These variables need to be bound to values on a per-mode basis.
(defvar semantic-ectags-lang nil
  "The language name used by Exuberant CTags for the current buffer.")
(defvar semantic-ectags-lang-kind nil
  "The kinds of tags fetched by Exuberant CTags for the current language.")
(defvar semantic-ectags-lang-extra-flags nil
  "Extra flags to pass to Exuberant CTags for a particular language.")
(defvar semantic-ectags-collect-errors nil
  "When non-nil, collect errors.")

(define-overloadable-function semantic-ectags-split-signature-summary (summary)
  "Split SUMMARY into Semantic tag compatible attributes.
SUMMARY is part of the output from Exuberant CTags that shows the
text from a file where the tag was found.")

(defun semantic-ectags-split-signature-summary-default (summary)
  "Default behavior for splitting a Exuberant CTags SUMMARY.
Assume comma separated list."
  (split-string summary "[(), ]" t))

(define-overloadable-function semantic-ectags-set-language-attributes (tag parents)
  "Augment TAG with additional attributes based on language.
PARENTS is the list of parent names for TAG.")

(defun semantic-ectags-set-language-attributes-default (tag parents)
  "Default behavior does nothing.
TAG and PARENTS are ignored."
  nil)

;;;###autoload
(defun semantic-ectags-parse-buffer ()
  "Execute Exuberant CTags on this buffer.
Convert the output tags into Semantic tags."
  (interactive)
  (require 'semantic/ectags/lang)
  (when (not semantic-ectags-lang)
    (error "Exuberant CTags support for Semantic not configured for %s"
           major-mode))
  (let* ((semantic-ectags-collect-errors (called-interactively-p 'any))
         (start (current-time))
         (tags
          (semantic-ectags-parse-file-with-mode (buffer-file-name) major-mode))
         (end (current-time)))

    (when (called-interactively-p 'any)
      (message "Parsed %d tags in %d seconds."
               (length tags)
               (semantic-elapsed-time start end))
      (data-debug-new-buffer (concat "*" (buffer-name) " ADEBUG*"))
      (data-debug-insert-tag-list tags "* ")
      (when (consp semantic-ectags-collect-errors)
        (insert "\n\nFound the following ectags config errors:\n")
        (dolist (E semantic-ectags-collect-errors)
          (insert "  * " E "\n")))
      )

    tags)
  )

(defun semantic-ectags-parse-file-with-mode (filename mode)
  "Execute Exuberant CTags on FILENAME using major mode MODE settings."
  (message "CTAGS/%s..." (file-name-nondirectory filename))
  (let* ((xtra (mode-local-symbol-value 'semantic-ectags-lang-extra-flags
                                        mode))
         (lang (mode-local-symbol-value 'semantic-ectags-lang
                                        mode))
         (kind (mode-local-symbol-value 'semantic-ectags-lang-kind
                                        mode))
         (arg-list (append
                    xtra
                    (list
                     (concat "--language-force=" lang)
                     "--sort=no"           ;; Don't resort the names.
                     "--excmd=number" ;; add line numbers
                     "--fields=aKStsim" ;; Add extra info
                     (format "--%s-kinds=%s" lang kind)
                     "-f" "-" ;; Send to standard out.
                     ;; We have to pass in the file, not buffer text.
                     filename)))
         (buff (apply 'semantic-ectags-run arg-list))

         )
    (with-current-buffer buff
      (condition-case nil
          ;; Sometimes this might throw an error.  Be safe.
          (funcall mode)
        (error (message "Error attempting to use mode settings with CTAGS.")))
      (semantic-ectags-parse-tags))
    ))

(defun semantic-ectags-parse-one-tag (line)
  "Split the Exuberant CTags LINE into a new tag.
Returns the list ( TAG P1 P2 Pn...)
where TAG is the new tag, P1, P2, and Pn is the list of
parents running forward, such as namespace/namespace/class"
  (let* ((elements (split-string line "\t"))
         (ect-class (nth 3 elements))
         (class-split (split-string ect-class " " t))
         (class (intern (car class-split)))
         (prototype nil)
         (const nil)
         (type nil)

         (class-sym (cond
                     ((member class '(function variable label program value))
                      class)
                     ((eq class 'prototype)
                      (setq prototype t)
                      'function)
                     ((member class '(namespace class trait struct union enum typedef))
                      (setq type (symbol-name class))
                      'type)
                     ((eq class 'member)
                      'variable)
                     ((member class '(include import))
                      'include)
                     ((or (eq class 'macro) (eq class 'define))
                      (setq const t)
                      'variable)
                     ((eq class 'enumerator)
                      (setq const t)
                      'variable)
                     ((eq class 'declaration)
                      'variable)
                     ((or (eq class 'subroutine) (eq class 'procedure))
                      ;; @todo - functions (the plural) for pascal?
                      'function)
                     ((eq class 'local)
                      ;; @TODO - local variables??
                      ;; These get assigned into other tags, and should
                      ;; be accessed via semantic-get-local-variables
                      'variable)
                     (t
                      (message "CTAG: Unknown output kind %s" class)
                      (when semantic-ectags-collect-errors
                        (let ((msg (format "Unknown class: %S" class)))
                          (if (eq semantic-ectags-collect-errors t)
                              (setq semantic-ectags-collect-errors (list msg))
                            (add-to-list 'semantic-ectags-collect-errors msg ))))
                      'unknown
                      )))

         (attr (semantic-ectags-split-fields (nthcdr 4 elements)))
         (line (string-to-number (nth 2 elements)))

         (tag (semantic-tag (nth 0 elements)
                            class-sym
                            ;; Leave filename for some other time
                            ;; :filename (nth 1 elements)
                            :line line
                            :prototype-flag prototype
                            :constant-flag const
                            :type (if (eq class-sym 'type) type nil) ;; Nil alows override later
                            ))
         (parents nil)
         )
    (if (eq class-sym 'unknown)
        ;; Unknown type.. let it through nicely.
        nil
      ;; Else, keep parsing.
      (while attr
        ;; Loop over each attribute, adding it into the tag.
        (cond ((eq (car attr) :parent)
               (setq parents (semantic-analyze-split-name (car (cdr attr))))
               (when (stringp parents)
                 (setq parents (list parents))))
              (t
               (semantic-tag-put-attribute tag (car attr) (car (cdr attr)))))
        (setq attr (cdr (cdr attr)))
        )
      ;; Now return the new tag.
      (cons tag parents)
      )))

(defun semantic-ectags-parse-tags ()
  "Parse the Exuberant CTags output in the current buffer."
  (goto-char (point-min))
  (let ((tags nil)
        (ptag-stack nil) ; parent tag stack.
        (pname nil)      ; parent names.
        )
    (while (not (eobp))
      (let* ((ptag (semantic-ectags-parse-one-tag
                    (buffer-substring-no-properties (point) (point-at-eol))))
             (tag (car ptag))
             (parents (cdr ptag))
             )

        (when ptag
          ;; Set some language specific attributes.
          (semantic-ectags-set-language-attributes tag parents)

          ;; At this point, we have to guess if TAG is embedded into one
          ;; of the parents in the parent stack.  There are three cases:
          ;;
          ;; 1) Old Lineage - "parent" matches pname exactly.
          ;;    --> embed into the end of the parent tag stack.
          ;; 2) Mixed Lineage - "parent" matches only part of the parent tag stack.
          ;;    --> Find the partial match, reset to there, and
          ;;        then embed the tag into the correct parent.
          ;; 3) New Lineage - "parent" does not match pname at all.
          ;;    --> Start over.
          ;;
          ;; Note that old/mixed/new lineage are a mixture of the same basic
          ;; algorithm to scan the list of known parents to find the match.
          ;;

          (if (not parents)
              (progn
                ;; Push the tag into our list.
                (push tag tags)

                (if (semantic-tag-of-class-p tag 'type)
                    ;; Merge stacks, and also
                    (setq ptag-stack (list tag)
                          pname (list (semantic-tag-name tag)))
                  ;; Flush embedded parantage
                  (setq ptag-stack nil
                        pname nil))
                )

            ;; If we have parents, lets look them up.
            (let ((oldnames pname)
                  (newnames parents)
                  (oldstack ptag-stack)
                  (newstack nil)
                  (add-to-this-parent nil)
                  (pushed-parent-list nil)
                  )
              (while (and oldstack (string= (car oldnames) (car newnames)))
                (setq newstack (cons (car oldstack) newstack)
                      oldstack (cdr oldstack)
                      oldnames (cdr oldnames)
                      newnames (cdr newnames)))

              ;; Push this tag into the last parent we found.
              (setq add-to-this-parent (car newstack))
              (setq pushed-parent-list newnames)

              ;; Do special stuff with type tags.
              (when (semantic-tag-of-class-p tag 'type)
                ;; Fill in the intermediate stack with NIL.
                (while newnames
                  (setq newnames (cdr newnames)
                        newstack (cons nil newstack)))
                ;; Add TAG to the end for matching the next tag in
                ;; the list.
                (setq newstack (cons tag newstack)))

              ;; Set back into ptag-stack.
              (setq ptag-stack (nreverse newstack))

              ;; Fix up the name list too
              (if (semantic-tag-of-class-p tag 'type)
                  (setq pname (append parents (list (semantic-tag-name tag))))
                (setq pname parents))

              ;; Set the lineage of the new tag.
              (if (not add-to-this-parent)
                  ;; No parent to add to.
                  (progn
                    (push tag tags)
                    (semantic-ectags-add-parent tag parents)
                    )
                ;; Add TAG to the correct parent, and save name
                (semantic-ectags-add-child add-to-this-parent tag)
                (semantic-ectags-add-parent tag pushed-parent-list)
                )
              )
            ))

        (end-of-line)
        (condition-case nil (forward-char 1) (error nil))))
    (nreverse tags)))

(defun semantic-ectags-add-child (parent child)
  "Add into the PARENT tag a new CHILD tag."
  (let ((children (semantic-tag-type-members parent))
        )
    (add-to-list 'children child t)
    (semantic-tag-put-attribute parent :members children)
    ))

(defun semantic-ectags-add-parent (tag parentlist)
  "Add to TAG the tag name in PARENTLIST."
  (when parentlist
    (let ((pstring (semantic-analyze-unsplit-name parentlist)))
      (semantic-tag-put-attribute tag :parent pstring)
      )))

(defun semantic-ectags-split-fields (fields)
  "Convert FIELDS into a list of Semantic tag attributes."
  (let ((attr nil))
    (dolist (F fields)
      (string-match "\\w+:" F)
      (let* ((me (match-end 0))
             (field (substring F 0 (1- me)))
             (str (substring F me))
             )
        (cond ((string= field "type")
               (push str attr)
               (push :type attr))
              ((string= field "line")
               (push (string-to-number str) attr)
               (push :line attr))
              ;; Class and Namespace seem to provide a name similar
              ;; to our :parent tag, so both should do that.
              ;; There is something extra here though..  It should
              ;; be possible to use this info to do a reparenting operation.
              ((or (string= field "class")
                   (string= field "struct"))
               (push str attr)
               (push :parent attr))
              ((string= field "namespace")
               (push str attr)
               (push :parent attr))
              ;;((string= field "inheritance")
              ;;(push str attr)
              ;;(push :parent attr)
              ;;)
              ((string= field "access")
               (push str attr)
               (push :protection attr))
              ((string= field "signature")
               (let ((sigattr (semantic-ectags-split-signature-summary str)))
                 (push sigattr attr)
                 (push :arguments attr)))
              ((string= field "implementation")
               (push str attr)
               (push :typemodifiers attr))
              (t
               (message "Unknown ectags field %s" field))))
      )
    attr))


;;; MAIN PARSER SUPPORT
;;
;; Tools for using ectags as the main parser for a language.

(defun semantic-ectags-setup-parse-table ()
  "Setup the current buffer for parsing with Exuberant CTags.
Unlike basic ECTags setup, this will setup the buffer so the main
parser is also using CTags to dynamically parse the buffer."
  (semantic-install-function-overrides
   '((parse-region . semantic-ectags-parse-region)
     (parse-changes . semantic-ectags-parse-changes)))
  (setq semantic-parser-name "CTAGS"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
        )
  )

(defun semantic-ectags-parse-region (&rest ignore)
  "Parse the current shell script buffer for semantic tags.
IGNORE any arguments, always parse the whole buffer."
  (let ((tags (semantic-ectags-parse-buffer))
        (newtags nil))
    (while tags
      (push (semantic-ectags-expand-tag (car tags)
                                       (car (cdr tags)))
            newtags)
      (setq tags (cdr tags)))
    (nreverse newtags)))

(defun semantic-ectags-parse-changes ()
  "Parse changes in the current shell script buffer."
  ;; NOTE: For now, just schedule a full reparse.
  ;;       To be implemented later.
  (semantic-parse-tree-set-needs-rebuild))

;;; TAG COOKING
;;
;; Tags are 'cooked' when they are bound into a buffer.
(defun semantic-ectags-expand-tag (tag nexttag)
  "Expand the Exuberant CTags TAG into the current buffer.
NEXTTAG provides a clue to the end of TAG.
CTags start out with a a line number.
Cooking a tag needs character positions instead.
NOTE: Currently this only supports a flat-list style tag."
  (let ((name (semantic-tag-name tag))
        (class (semantic-tag-class tag))
        (attr (semantic-tag-attributes tag))
        (line nil)
        (newattr nil)
        start end)
    (while attr
      (if (eq (car attr) :line)
          (setq line (car (cdr attr)))
        (push (car (cdr attr)) newattr)
        (push (car attr) newattr))
      (setq attr (cdr (cdr attr))))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (setq start (point-at-bol)
            end
            (progn
              (if nexttag
                  (progn
		    (goto-char (point-min))
		    (forward-line (1- (semantic-tag-get-attribute nexttag :line))))
                (goto-char (point-max))
                )
              (while (forward-comment -1) nil)
              (point)
              )))
    ;; We can safely take the first because ectags
    ;; doesn't produce compound tags.
    (let ((ret
           (car
            (semantic--tag-expand
             ;; Uncooked tags are unlike standard tags.
             (list name class newattr nil nil start end)))
           ))
      ret)))

(provide 'semantic/ectags/parse)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/ectags/parse"
;; End:

;;; semantic/ectags/parse.el ends here
