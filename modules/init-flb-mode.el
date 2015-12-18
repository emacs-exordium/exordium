;;; -*- lexical-binding: t -*-
;;; Defines a minor mode for making buffers local to each frame.
;;
;; This mode adds the notion of buffer ownership to Emacs frames. If you use
;; many frames (for example with emacsclient), each frame will show a reduced
;; list of buffers - the ones that you opened in that frame - instead of all
;; buffers.
;;
;; M-x `flb-mode' toggles the frame-local-buffers mode. When this mode is on,
;; any function for selecting buffers such as `C-x b' will only show the
;; buffers that have been open within that frame. In addition, closing a frame
;; automatically closes all buffers that were only open within that frame.
;;
;; Usage example:
;; 1. Open foo.txt
;; 2. Turn on flb-mode
;; 3. Open a new frame with `C-x 5 2', which will also display foo.txt
;; 4. Open bar.txt in the second frame
;; 5. Do `C-x b' in both frames: foo.txt will show up in both, but bar.txt
;;    will only appear in the second frame.
;; 6. Close the second frame: bar.txt is automatically closed. You can verify
;;    that by disabling flb-mode (to show all buffers).
;;
;; Note: if multiple frames are open when flb-mode is turned on, all these
;; frames share all existing buffers, meaning they show in each frame's buffer
;; list. Also note that this mode only deals with buffer visibility: killing a
;; buffer kills it for all frames.
;;
;; Implementation: flb-mode uses a variable that associates a list of buffers
;; to each frame. When the mode is enabled it replaces the definition of
;; function `buffer-list' by its own function, so that any function that uses
;; `buffer-list' (such as IDO) will work correctly. flb-mode also adds a hook
;; that is executed after each command: it is used to detect if new frames or
;; buffers are open or killed in order to maintain the variable. The hook has a
;; very small cost.
;;
;; TODO: implement support for bury-buffer in FLB mode

(require 'cl)

(define-minor-mode flb-mode
  "Frame-local buffer mode. This macro defines both a function
and variable `flb-mode'."
  ;; Initial value:
  nil
  ;; Indicator for the mode line
  "FLB"
  ;; Key bindings (none):
  nil
  ;; Flags:
  :group 'exordium
  :global t
  ;; Body of the `flb-mode' function:
  (if flb-mode
      (flb-activate)
    (flb-deactivate)))

(defvar flb-frame-buffers ()
  "Lists of buffers owned by each frame. This is a plist:
'(frame1 (buffer1 buffer2) frame2 (buffer3))")

(defvar flb-last-frame nil
  "Last selected frame, or nil if flb-mode is disabled")

(defvar flb-last-buffer nil
  "Last selected buffer, or nil if flb-mode is disabled")

(defun common-buffer-list (orig-fun)
  "Using `orig-fun' as a function that retuns a list of
buffers (e.g. `buffer-list'), returns a subset of this list that
contains all common buffers, e.g. the buffers that should be
shared among all frames."
  (let ((buffers (funcall orig-fun)))
    (loop for b in buffers
          when (let ((n (buffer-name b)))
                 (and (string-prefix-p "*" n)
                      (string-suffix-p "*" n)))
          collect b)))

(defun flb-buffer-list (orig-fun &rest args)
  "Advice around `buffer-list': returns the list of buffers. Note
that this advice disregards the `orig-fun'."
  (cl-remove-duplicates
   (append (plist-get flb-frame-buffers (selected-frame))
           (common-buffer-list orig-fun))))

(defun flb-activate (&optional silent)
  "Activates the frame-local-buffers mode"
  ;; Make each existing frame share all existing buffers
  (dolist (f (frame-list))
    (setq flb-frame-buffers (plist-put flb-frame-buffers f (buffer-list))))
  ;; Replace `buffer-list' with our own function and add a post-command hook
  (advice-add 'buffer-list :around #'flb-buffer-list)
  (add-hook 'post-command-hook #'flb-post-command-hook)
  (unless silent
    (message "Frame-local-buffers mode is enabled")))

(defun flb-deactivate (&optional silent)
  "Deactivates the frame-local-buffers mode"
  ;; Restore the original `buffer-list' and remove our hook
  (advice-remove 'buffer-list #'flb-buffer-list)
  (remove-hook 'post-command-hook #'flb-post-command-hook)
  ;; Clear variables
  (setq flb-frame-buffers nil
        flb-last-frame nil
        flb-last-buffer nil)
  (unless silent
    (message "Frame-local-buffers mode is disabled")))

(defun flb-frame-owns-buffer-p (frame buffer)
  "Predicate testing if FRAME owns BUFFER, e.g. if BUFFER is only
  open in FRAME"
  (loop for (key value) on flb-frame-buffers by #'cddr
        ;; skip FRAME
        unless (eq key frame)
        ;; return t if BUFFER is never a member of value
        never (member buffer value)))

(defun flb-remove-dead-frames ()
  "Removes any dead frames from variable `flb-frame-buffers'"
  (let ((newlist ()))
    (loop for (key value) on flb-frame-buffers by #'cddr
          do (if (frame-live-p key)
                 ;; Keep the frame
                 (setq newlist (plist-put newlist key value))
               ;; else don't keep it and kill any buffers it owns
               (dolist (b value)
                 (when (flb-frame-owns-buffer-p key b)
                   (kill-buffer b)))))
    (setq flb-frame-buffers newlist)))

(defun flb-update-frame-buffers (frame buffer)
  "Updates variable `flb-frame-buffers' using the new current
  FRAME and BUFFER: adds BUFFER to the front of FRAME's local
  buffers and removes any dead buffers."
  (let ((local-buffers (plist-get flb-frame-buffers frame)))
    (cl-pushnew buffer local-buffers)
    (setq local-buffers (remove-if-not #'buffer-live-p local-buffers))
    (setq flb-frame-buffers (plist-put flb-frame-buffers frame local-buffers))))

(defun flb-post-command-hook ()
  "Hook function that runs after every command and updates the
  frame-buffer associations."
  (let ((f (selected-frame))
        (b (current-buffer)))
    ;; Update associations if either the frame or the buffer has changed:
    (when (not (eq f flb-last-frame))
      (flb-remove-dead-frames))
    (when (or (not (eq f flb-last-frame))
              (not (eq b flb-last-buffer)))
      (flb-update-frame-buffers f b))
    (setq flb-last-frame f
          flb-last-buffer b)))

;;; Quick workaround to make magit work when FLB mode is turned on.
;;; TODO: there must be a better way...

(when (fboundp 'magit-save-repository-buffers)
  (defadvice magit-save-repository-buffers (around magit-disable-flb activate)
    (let ((previously-on flb-mode))
      (when flb-mode (flb-deactivate t))
      ad-do-it
      (when previously-on (flb-activate t)))))

(provide 'init-flb-mode)
