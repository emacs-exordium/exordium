;;; init-elisp.t.el --- Unit tests for init-elisp.t.el -*- lexical-binding: t -*-
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert


;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-elisp)
(exordium-require 'init-flycheck)

(use-package el-mock
  :ensure t
  :autoload (mocklet
             mocklet-function))
(require 'ert)
(require 'rx)

(ert-deftest exordium--pp-output-setup-nothing-in-random-buffer ()
  (with-temp-buffer
    (insert "(foo)")
    (emacs-lisp-mode)
    (exordium--pp-output-setup)
    (goto-char (point-min))
    (should-not (looking-at (rx string-start
                                ";; -*- lexical-binding: t -*-")))
    (should-not (memq 'emacs-lisp-checkdoc flycheck-disabled-checkers))))


(ert-deftest exordium--pp-output-setup-add-header-and-disable-flycheck ()
  (dolist (name '("*Pp Eval Output*" "*Pp Macroexpand Output*"))
    (eval `(mocklet ((buffer-name => ,name))
             (with-temp-buffer
               (insert "(foo)")
               (emacs-lisp-mode)
               (exordium--pp-output-setup)
               (goto-char (point-min))
               (should (looking-at (rx string-start
                                       ";; -*- lexical-binding: t -*-")))
               (should (memq 'emacs-lisp-checkdoc flycheck-disabled-checkers)))))))

(provide 'init-elisp.t)

;;; init-elisp.t.el ends here
