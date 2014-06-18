;;; ecb-semantic.el -- define tag-utilities based on semantic

;; Copyright (C) 2000 - 2009 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2009

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$

;;; Commentary:

;; This file contains some tag-utility functions based on semantic (part of
;; CEDET-suite) which are needed by ECB


(require 'ecb-cedet-wrapper)
(require 'ecb-util)

(eval-when-compile
  (require 'silentcomp))

(defun ecb-semanticdb-find-result-nth-with-file (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0. Returns
a cons cell with car is the searched and found tag and the cdr is the
associated full filename of this tag. If the search result is not associated
with a file, then the cdr of the result-cons is nil."
  (let ((result-nth (ecb--semanticdb-find-result-nth result n)))
    (if (and (car result-nth)
             (ecb--semantic-tag-with-position-p (car result-nth))
             (cdr result-nth))
        (cons (car result-nth)
              (ecb--semanticdb-full-filename (cdr result-nth)))
      (cons (car result-nth) nil))))

(defun ecb-get-definition-list-by-semanticdb (tag-name &optional tag-class)
  "Search for the definitions of the tag with TAG-NAME and TAG-CLASS.
The search is performed via semanticdb.
`semanticdb-search-system-databases' is taken into account.
Return-value is either nil \(if no positioned tag can be found
for TAG-NAME and TAG-CLASS) or a positioned semantic-tag for the
type-definition of TAG-NAME.

If TAG-CLASS is nil then tags regardless of their class are returned as long
as they match with TAG-NAME."
  (when (and (featurep 'semanticdb) (ecb--semanticdb-minor-mode-p))
    ;; With semantic 2.X we do a full featured database-search.
    (let* ((search-result (ecb--semanticdb-find-tags-by-name tag-name))
           (result-tags (and search-result
                             (ecb--semanticdb-strip-find-results search-result)))
           (tag-numbers nil))
      (when (and result-tags
                 ;; some paranoia
                 (= (length result-tags)
                    (ecb--semanticdb-find-result-length search-result)))
        ;; First we check which tags in the stripped search-result
        ;; (result-tags) are of the right tag-class and with positions (means
        ;; associated with a file) and collect their sequence-positions in
        ;; tag-numbers.
        (dotimes (i (length result-tags))
          (if (and (or (null tag-class) ;; we are interested in any tags regardless of class
                       (equal (ecb--semantic-tag-class (nth i result-tags)) tag-class))
                   (ecb--semantic-tag-with-position-p (nth i result-tags)))
              (setq tag-numbers (cons i tag-numbers))))
        (setq tag-numbers (nreverse tag-numbers))
        ;; Now we get for each element in tag-numbers the related
        ;; filename (where the tag is defined) and collect them in an alist
        ;; where each element is a cons-cell where car is the filename and
        ;; cdr is the tag in this file. Especially with scoped languages
        ;; like C++ or Java a type with the same name can be defined in more
        ;; than one file - each of these files belonging to another
        ;; package/library.
        (delq nil
              (mapcar (function (lambda (n)
                                  (let ((r (ecb-semanticdb-find-result-nth-with-file
                                            search-result n)))
                                    (if (and (cdr r)
                                             (stringp (cdr r))
                                             (file-readable-p (cdr r)))
                                        (cons (cdr r) (car r))))))
                      tag-numbers))))))

(defun ecb-search-tag-by-semanticdb (tag-name &optional tag-class)
  "Uses semanticdb to search for the definition of tag with TAG-NAME and TAG-CLASS.
Return exactly one semantic tag for the definition of TAG-NAME.
If more than one definition have been found then the user has to
make a choice on file-basis.
If TAG-CLASS is nil then a tag regardless of its class is returned as long
as it matches with TAG-NAME.

The returned tag will contain the filename as its :file-attribute."
  (let ((definition-alist (ecb-get-definition-list-by-semanticdb tag-name tag-class))
        (result-elem))
    (when definition-alist
      ;; if we got more than one file for tag-name then the user has to
      ;; choose one.
      (setq result-elem
            (if (> (length definition-alist) 1)
                (assoc (ecb-offer-choices "Select a definition-file: "
                                          (mapcar #'car definition-alist))
                       definition-alist)
              (car definition-alist)))
      ;; we add the filename to the tag, then all needed informations are
      ;; within the tag
      (ecb--semantic--tag-put-property (cdr result-elem)
                                       :filename
                                       (car result-elem)))))

(defun ecb-search-tag-by-semantic-analyzer (tag-name &optional tag-class)
  "Calculate scope at point and search for a tag-definition with TAG-NAME.

If TAG-CLASS is not nil then only tags with this tag-class are searched.

Return either a positioned semantic-tag for the found
tag-definition or nil if nothing is found. This mechanism uses
the semantic-analyzer. Therefore it will work at its best if all
needed customizations for the semantic analyzer have been done.
\(See the manual of the semantic analyzer for how to customizing
it)."
  (let* ((scope (ecb--semantic-calculate-scope)))
    (when scope
      (ecb--semantic-analyze-find-tag tag-name tag-class scope))))

(silentcomp-provide 'ecb-semantic)

;;; ecb-semantic.el end here
