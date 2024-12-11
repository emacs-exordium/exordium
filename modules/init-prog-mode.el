;;; init-prog-mode.el --- Shared prog-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'newcomment)
(require 'prog-mode)

(use-package cmake-mode
  :defer t)

(define-minor-mode exordium-show-trailing-whitespace-mode
  "Enable `show-trailing-whitespace'."
  :init-value nil
  :lighter nil
  (progn (setq show-trailing-whitespace exordium-show-trailing-whitespace-mode)))

(define-minor-mode exordium-require-final-newline-mode
  "Enables `require-final-newline'."
  :init-value nil
  :lighter nil
  (progn (setq require-final-newline exordium-require-final-newline-mode)))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'exordium-show-trailing-whitespace-mode)
(add-hook 'prog-mode-hook #'exordium-require-final-newline-mode)

(use-package flyspell
  :ensure nil
  :if (eq exordium-spell-check :prog)
  :diminish flyspell-mode
  :hook
  (prog-mode . flyspell-prog-mode)
  :bind
  ;; unbind as it colides with company, see init-company.el
  (:map flyspell-mode-map
   ("C-." . nil)))

;;; Electric pair: automatically close parenthesis, curly brace etc.
;;; `electric-pair-open-newline-between-pairs'.
(setq electric-pair-open-newline-between-pairs t)
(when exordium-enable-electric-pair-mode
  (add-hook 'prog-mode-hook #'electric-pair-mode))

;;; The return key
(cond (exordium-enable-newline-and-indent
       (bind-key "RET" #'newline-and-indent prog-mode-map)
       (bind-key "S-RET" #'newline prog-mode-map))
      (t
       (bind-key "S-RET" #'newline-and-indent prog-mode-map)))


;;; Fill comments, comment regions
(setq comment-auto-fill-only-comments 1)
(bind-key "C-c C-c" #'comment-dwim prog-mode-map)

;;; Step through compile errors
(bind-key "<f10>" #'next-error)
(bind-key "C-<f10>" #'previous-error)


;;; Font lock changes

(defun exordium--add-keywords-for-todos ()
  "Display TODO: and FIXME: and TBD: in warning face."
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend))))

(when exordium-font-lock
  (add-hook 'prog-mode-hook #'exordium--add-keywords-for-todos))

(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
