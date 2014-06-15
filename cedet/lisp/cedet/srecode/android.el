;;; srecode/android.el --- SRecoder support for Android projects
;;
;; Copyright (C) 2011 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
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
;; Specialized support for developing Android applications with SRecode.

;;; Code:

;;;###autoload
(defun srecode-semantic-handle-:android (dict)
  "Add android specific symbols into DICT based on the current project."
  nil)

(defun srecode-android-read-resource-id (prompt &optional initial-input history default-value)
  "Like completing read for Android resource id names.
PROMPT is a string prompting the user in the minibuffer.
INITIAL-INPUT is the default value to display in the minibuffer.
HISTORY is a history variable to use.
DEFAULT-VALUE is the value to return if the user hits RET without typing anything."
  ;; @TODO - Convert to completing read.
  (read-string prompt initial-input history default-value))

(provide 'srecode/android)

;;; srecode-android.el ends here
