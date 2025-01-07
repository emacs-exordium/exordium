;;; init-highlight.t.el --- Unit tests for init-highlight.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-highlight)

(require 'ert)

(ert-deftest test-exordium--extract-font-lock-keywords ()
  (let ((pattern (exordium--extract-font-lock-keywords lisp-el-font-lock-keywords-1)))
    (should (string-match-p pattern "defun"))
    (should-not (string-match-p pattern "xdefun"))
    (should-not (string-match-p pattern "defunx"))
    (should (string-match-p pattern "cl-defun"))
    (should-not (string-match-p pattern "xcl-defun"))
    (should-not (string-match-p pattern "cl-defunx"))))


(ert-deftest test-exordium-hex-colors-font-lock-keywords ()
  (let ((regexp (caar exordium-hex-colors-font-lock-keywords)))
    (should (string-match-p regexp " #1Aa"))
    (should (string-match-p regexp " #Aa12Bb"))
    (should (string-match-p regexp " #a1ABb2cC3"))
    (should (string-match-p regexp " #1aAb2BC3cDd4"))
    (should-not (string-match-p regexp "&#1Aa"))
    (should-not (string-match-p regexp "&#Aa12Bb"))
    (should-not (string-match-p regexp "&#a1ABb2cC3"))
    (should-not (string-match-p regexp "&#1aAb2BC3cDd4"))))


(provide 'init-highlight.t)

;;; init-highlight.t.el ends here
