;;; hlinum.el --- Extension for linum.el to highlight current line number

;; Copyright (C) 2011-2014  by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: http://code.google.com/p/hlinum-mode/
;; Package-Requires: ((cl-lib "0.2"))
;; Keywords: convenience, extensions

;; This program is free software; you can redistribute it and/or modify
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

;; Extension for linum-mode to highlight current line number.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'hlinum)
;;     (hlinum-activate)
;; And by using M-x linum-mode, you can see line numbers
;; with highlighting current line number.
;;
;; You can customize the color of highlighting current line by
;; changing `linum-highlight-face'.
;; By default, hlinum highlights current line only in the active buffer.
;; To highlight current line in all buffers, change
;; `linum-highlight-in-all-buffersp' to t.

;;; Code:

(require 'linum)
(require 'cl-lib)

(defface linum-highlight-face
    '((t (:inherit default :foreground "black"
          :background "gray")))
  "Face for highlighting current line"
  :group 'linum)

(defcustom linum-highlight-in-all-buffersp
  nil
  "Non-nil means hlinum highlights current line in all buffers.
Otherwise hlinum will highlight only in the active buffer."
  :type 'boolean
  :group 'linum)

(defun hlinum-color (face)
  "Highlight current line number by using face FACE."
  (save-excursion
    (let* ((pt (max (window-start)
                    (progn (move-beginning-of-line nil)
                           (point))))
           (ov (cl-find-if
                (lambda (e) (stringp (overlay-get e 'linum-str)))
                (overlays-in pt pt))))
      (when ov
        (let* ((str (overlay-get ov 'before-string))
               (lstr (overlay-get ov 'linum-str))
               (nov (move-overlay ov pt pt)))
          (add-text-properties 0 (string-width lstr)
                               `(face ,face) lstr)
          (add-text-properties 0 1 `(display ((margin left-margin)
                                              ,lstr)) str)
          (overlay-put nov 'before-string str)
          (overlay-put nov 'linum-str lstr))))))

(defun hlinum-highlight-current-line ()
  (hlinum-color 'linum-highlight-face))
(defun hlinum-unhighlight-current-line ()
  (unless linum-highlight-in-all-buffersp
    (hlinum-color 'linum)))

(defadvice linum-update-current (after linum-aft-cur)
  (hlinum-highlight-current-line))
(defadvice linum-after-size (after linum-aft-size)
  (hlinum-highlight-current-line))
(defadvice linum-after-scroll (after linum-aft-scl)
  (when (eq (current-buffer) (window-buffer))
    (hlinum-highlight-current-line)))

;;;###autoload
(defun hlinum-activate ()
  "Enable highlighting current line number."
  (interactive)
  (ad-activate 'linum-update-current 'linum-aft-cur)
  (ad-activate 'linum-after-size 'linum-aft-size)
  (ad-activate 'linum-after-scroll 'linum-aft-scl)
  (add-hook 'pre-command-hook 'hlinum-unhighlight-current-line))

;;;###autoload
(defun hlinum-deactivate ()
  "Disable highlighting current line number."
  (interactive)
  (remove-hook 'pre-command-hook 'hlinum-unhighlight-current-line)
  (ad-deactivate 'linum-update-current)
  (ad-deactivate 'linum-after-size)
  (ad-deactivate 'linum-after-scroll))

(provide 'hlinum)
;;; hlinum.el ends here
