;;; git-timemachine.el --- Walk through git revisions of a file

;; Copyright (C) 2014 Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 20140725.501
;; X-Original-Version: 1.4
;; URL: https://github.com/pidu/git-timemachine
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: git

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Use git-timemachine to browse historic versions of a file with p
;;; (previous) and n (next).

(require 'cl-lib)

;;; Code:

(defvar git-timemachine-directory nil)
(make-variable-buffer-local 'git-timemachine-directory)
(defvar git-timemachine-file nil)
(make-variable-buffer-local 'git-timemachine-file)
(defvar git-timemachine-revision nil)
(make-variable-buffer-local 'git-timemachine-revision)

(defun git-timemachine--revisions ()
 "List git revisions of current buffers file."
 (let ((default-directory git-timemachine-directory))
  (process-lines "git" "log" "--pretty=format:%h" git-timemachine-file)))

(defun git-timemachine-show-current-revision ()
 "Show last (current) revision of file."
 (interactive)
 (git-timemachine-show-revision (car (git-timemachine--revisions))))

(defun git-timemachine-show-previous-revision ()
 "Show previous revision of file."
 (interactive)
 (git-timemachine-show-revision (cadr (member git-timemachine-revision (git-timemachine--revisions)))))

(defun git-timemachine-show-next-revision ()
 "Show next revision of file."
 (interactive)
 (git-timemachine-show-revision (cadr (member git-timemachine-revision (reverse (git-timemachine--revisions))))))

(defun git-timemachine-show-revision (revision)
 "Show a REVISION (commit hash) of the current file."
 (when revision
  (let ((current-position (point)))
   (setq buffer-read-only nil)
   (erase-buffer)
   (let ((default-directory git-timemachine-directory))
    (call-process "git" nil t nil "show" (concat revision ":" git-timemachine-file)))
   (setq buffer-read-only t)
   (set-buffer-modified-p nil)
   (let* ((revisions (git-timemachine--revisions))
          (n-of-m (format "(%d/%d)" (- (length revisions) (cl-position revision revisions :test 'equal)) (length revisions))))
    (setq mode-line-format (list "Commit: " revision " -- %b -- " n-of-m " -- [%p]")))
   (setq git-timemachine-revision revision)
   (goto-char current-position))))

(defun git-timemachine-quit ()
 "Exit the timemachine."
 (interactive)
 (kill-buffer))

(defun git-timemachine-kill-revision ()
 "Kill the current revisions commit hash."
 (interactive)
 (message git-timemachine-revision)
 (kill-new git-timemachine-revision))

(define-minor-mode git-timemachine-mode
 "Git Timemachine, feel the wings of history."
 :init-value nil
 :lighter " Timemachine"
 :keymap
 '(("p" . git-timemachine-show-previous-revision)
   ("n" . git-timemachine-show-next-revision)
   ("q" . git-timemachine-quit)
   ("w" . git-timemachine-kill-revision))
 :group 'git-timemachine)

;;;###autoload
(defun git-timemachine ()
 "Enable git timemachine for file of current buffer."
 (interactive)
 (let ((git-directory (file-name-as-directory (car (process-lines "git" "rev-parse" "--show-toplevel"))))
       (file-name (buffer-file-name))
       (timemachine-buffer (format "timemachine:%s" (buffer-name)))
       (mode major-mode))
  (with-current-buffer (get-buffer-create timemachine-buffer)
   (setq buffer-file-name file-name)
   (funcall mode)
   (git-timemachine-mode)
   (setq git-timemachine-directory git-directory
         git-timemachine-file (file-relative-name file-name git-directory)
         git-timemachine-revision nil)
   (git-timemachine-show-current-revision)
   (switch-to-buffer timemachine-buffer))))

(provide 'git-timemachine)

;;; git-timemachine.el ends here
