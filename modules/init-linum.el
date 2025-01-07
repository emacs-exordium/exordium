;;; init-linum.el --- Line numbers                   -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The number one thing that can make Emacs slow is the line number mode.
;; This module turn it on for all modes except the ones that for which we
;; don't need it (e.g. using an exception list).
;;
;; See also init-ui.el

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'display-line-numbers)

(defun exordium--inhibit-line-numbers-p ()
  "Return non nil if line numbers should be inhibited in current buffer.
Otherwise return nil."
  (or (minibufferp)
      (and exordium-inhibit-line-numbers-modes
           (cl-find-if (lambda (mode)
                         (derived-mode-p mode))
                       exordium-inhibit-line-numbers-modes))
      (and exordium-inhibit-line-numbers-star-buffers
           (string-match (rx string-start "*") (buffer-name)))
      (and exordium-inhibit-line-numbers-buffer-size
           (> (buffer-size) exordium-inhibit-line-numbers-buffer-size))))

;;;###autoload
(define-globalized-minor-mode exordium-global-display-line-numbers-mode
  display-line-numbers-mode
  (lambda () (unless (exordium--inhibit-line-numbers-p)
               (display-line-numbers-mode)))
  :group 'exordium)

(when exordium-display-line-numbers
  (exordium-global-display-line-numbers-mode t))

(provide 'init-linum)

;;; init-linum.el ends here
