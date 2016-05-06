;;;; Progress bar.
;;;
;;; This module provides a function `update-progress-bar' which display the
;;; progress of init.el. This function is supposed to be called at various
;;; intervals of init.el. Constant `exordium-loading-step-count' must be set to
;;; the number of steps, e.g. the number of times `update-progress-bar' will be
;;; called.
;;;
;;; This idea was shamelessly borrowed from Spacemacs.
;;;
;;; Note: this is just for fun. It actually makes the configuration load slower.

(require 'init-prefs)

;;; Set this constant to the number of times `update-progress-bar' is called in
;;; init.el:
(defconst exordium-loading-step-count 6)

;;; Don't change any of these:
(defconst exordium-loading-step-size
  (/ (window-total-size nil 'width) exordium-loading-step-count))
(defconst exordium-loading-char ?█)
(defvar exordium-loading-string "")
(defvar exordium-start-time (current-time))

(defun update-progress-bar ()
  "Add one more step to the progress bar"
  (when exordium-progress-bar
    ;; Use this for debugging, each step should take approximately the same time
    ;; (message "update-progress-bar: %s"
    ;;          (format "%.1fs" (float-time (time-subtract (current-time) exordium-start-time))))
    (setq exordium-loading-string
          (concat exordium-loading-string
                  (make-string exordium-loading-step-size
                               exordium-loading-char)))
    (setq mode-line-format exordium-loading-string)
    (redisplay)))

(provide 'init-progress-bar)
