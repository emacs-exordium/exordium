;;;; GDB
;;;
;;; Shortcuts:
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html
;;;
;;; Functions:
;;;
;;; - M-x `gdb-few-windows': split the current frame into 3 windows stacked up
;;;   on top of each other: gdb command line, source code, program IO.
;;;   This function completes the native `gdb-many-windows' which displays 6
;;;   windows (stack, breakpoints, local variables or registers).

(require 'gdb-mi)

;; Show main source buffer when using GDB.
(setq gdb-show-main t)

;; Highlight changed variables using the error face.
(setq gdb-show-changed-values t)

;;; Keep the current line in sync with the point and in the center of the
;;; buffer. Otherwise the current line may disappear from the buffer as you step
;;; into the code. I don't know why this is not the default.
(defadvice gud-display-line (after gud-display-line-centered activate)
  "Center the current line in the source code window"
  (when (and gud-overlay-arrow-position gdb-source-window)
    (with-selected-window gdb-source-window
      ; (marker-buffer gud-overlay-arrow-position)
      (save-restriction
        ;; Compiler-happy equivalent to (goto-line (ad-get-arg 1))
        (goto-char (point-min))
        (forward-line (1- (ad-get-arg 1)))
        (recenter)))))

(defun gdb-few-windows ()
  "Slit the current frame into 3 windows: gdb command line,
source code, and program IO."
  (interactive)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4)))
        (win2 (split-window nil ( / (window-height) 3))))
    ;; IO
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win1)
    ;; Source code or current buffer if not found.
    (set-window-buffer
     win2
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put the buffer list in window if we cannot find a source file...
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    ;; Give focus to the comint window.
    (select-window win0)))

(provide 'init-gdb)
