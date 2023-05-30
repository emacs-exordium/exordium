;;; Unit tests for init-util.el.
;;; To run all tests:
;;;     M-x eval-buffer
;;;     M-x ert

(require 'init-util)
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

;; Local Variables:
;; no-byte-compile: t
;; End:
