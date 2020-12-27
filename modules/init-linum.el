;;;; Line numbers
;;;
;;; The number one thing that can make Emacs slow is the line number mode.
;;; This module turn it on for all modes except the ones that for which we
;;; don't need it (e.g. using an exception list).
;;;
;;; See also init-ui.el

(require 'init-prefs)
(use-package display-line-numbers :ensure nil)

(defun exordium--inhibit-line-numbers-p ()
  (or (minibufferp)
      (and exordium-inhibit-line-numbers-modes
           (cl-find-if #'(lambda (mode)
                           (derived-mode-p mode))
                       exordium-inhibit-line-numbers-modes))
      (and exordium-inhibit-line-numbers-star-buffers
           (eq 0 (string-match "*" (buffer-name))))
      (and exordium-inhibit-line-numbers-buffer-size
           (> (buffer-size) exordium-inhibit-line-numbers-buffer-size))))

;;;###autoload
(define-globalized-minor-mode exordium-global-display-line-numbers-mode
  display-line-numbers-mode
  (lambda () (unless (exordium--inhibit-line-numbers-p)
               (display-line-numbers-mode))))

(when exordium-display-line-numbers
  (exordium-global-display-line-numbers-mode t))

(provide 'init-linum)
