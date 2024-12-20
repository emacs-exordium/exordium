;;; init-lib.t.el --- Unit tests for init-util.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-util)

(require 'ert)


(ert-deftest test-exordium-setf-when-nil-empty-alist ()
  (should
   (equal '((a . 1))
          (let (alist)
            (should
             (equal (exordium-setf-when-nil (alist-get 'a alist) 1)
                    1))
            alist))))

(ert-deftest test-exordium-setf-when-nil-elt-in-alist ()
  (should
   (equal '((a . 1))
          (let ((alist '((a . 1))))
            (should
             (equal (exordium-setf-when-nil (alist-get 'a alist)
                                            (ert-fail "'a is in alist - should not eval"))
                    1))
            alist))))

(ert-deftest test-exordium-setf-when-nil-a-new-elt-to-alist ()
  (should
   (equal '((b . 2) (a . 1))
          (let ((alist '((a . 1))))
            (should
             (equal (exordium-setf-when-nil (alist-get 'b alist) 2)
                    2))
            alist))))

(ert-deftest test-exordium-setf-when-nil-elt-in-alist-and-a-new-elt ()
  (should
   (equal '((b . 2) (a . 1))
          (let ((alist '((a . 1))))
            (should
             (equal (exordium-setf-when-nil (alist-get 'a alist)
                                            (ert-fail "'a is in alist - should not eval")
                                            (alist-get 'b alist)
                                            2)
                    2))
            alist))))

(ert-deftest test-exordium-setf-when-nil-a-new-elt-and-elt-in-alist ()
  (should
   (equal '((b . 2) (a . 1))
          (let ((alist '((a . 1))))
            (should
             (equal (exordium-setf-when-nil (alist-get 'b alist)
                                            2
                                            (alist-get 'a alist)
                                            (ert-fail "'a is in alist - should not eval"))
                    1))
            alist))))

(provide 'init-lib.t)

;;; init-lib.t.el ends here
