;;; init-dired.el --- Configuration of dired, dired+, and wdired -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package dired
  :ensure nil
  :commands (dired-mouse-find-file-other-window)
  :bind
  (("C-x C-j" . #'dired-jump)
   ("C-x 4 C-j" . #'dired-jump-other-window)
   :map dired-mode-map
   ("<mouse-3>" . #'dired-mouse-find-file-other-window)))

(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t))

(use-package dired-x
  :ensure nil
  :custom
  (dired-x-hands-off-my-keys nil))

(use-package find-dired
  :ensure nil
  :custom
  ;; xargs to get options rather than exec ls on each find
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(provide 'init-dired)

;;; init-dired.el ends here
