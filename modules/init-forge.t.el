;;; Unit tests for init-forge.el.
;;; To run all tests:
;;;     M-x eval-buffer
;;;     M-x ert

(require 'init-forge)
(require 's)
(require 'ert)
(require 'cl-lib)

;; Tests for `exordium-forge-cleanup-known-repositories--question'

(ert-deftest test-exordium-forge-cleanup-known-repositories--question-messages-for-all ()
  (let* ((to-delete '((1 1 1 1) (2 2 2 2) (3 3 3 3)))
         (message-call-args ""))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (setq message-call-args
                       (s-append (cadr args) message-call-args)))))
      (exordium-forge-cleanup-known-repositories--question to-delete 1))
    (should (s-contains-p "1/1 @1" message-call-args))
    (should (s-contains-p "2/2 @2" message-call-args))
    (should (s-contains-p "3/3 @3" message-call-args))))

(ert-deftest test-exordium-forge-cleanup-known-repositories--question-plural-for-two-more ()
  (let* ((to-delete '((1 1 1 1) (2 2 2 2) (3 3 3 3)))
         (question (exordium-forge-cleanup-known-repositories--question to-delete 1)))
    (should (s-contains-p "1/1 @1" question))
    (should (s-contains-p "and 2 other repositories" question))
    (should-not (s-contains-p "2/2 @2" question))
    (should-not (s-contains-p "3/3 @3" question))))

(ert-deftest test-exordium-forge-cleanup-known-repositories--question-singular-for-one-more ()
  (let* ((to-delete '((1 1 1 1) (2 2 2 2)))
         (question (exordium-forge-cleanup-known-repositories--question to-delete 1)))
    (should (s-contains-p "1/1 @1" question))
    (should (s-contains-p "and 1 other repository" question))
    (should-not (s-contains-p "2/2 @2" question))))

(ert-deftest test-exordium-forge-cleanup-known-repositories--question-default-number-is-5 ()
  (let* ((to-delete '((1 1 1 1) (2 2 2 2) (3 3 3 3)
                      (4 4 4 4) (5 5 5 5) (6 6 6 6)))
         (question (exordium-forge-cleanup-known-repositories--question to-delete)))
    (should (s-contains-p "1/1 @1" question))
    (should (s-contains-p "2/2 @2" question))
    (should (s-contains-p "3/3 @3" question))
    (should (s-contains-p "4/4 @4" question))
    (should (s-contains-p "5/5 @5" question))
    (should (s-contains-p "and 1 other repository" question))
    (should-not (s-contains-p "6/6 @6" question))))

;; Tests for `exordium-forge-cleanup-known-repositories--concat'

(ert-deftest test-exordium-forge-cleanup-known-repositories--concat-1 ()
  (let ((to-delete '(("worktree-1" "host-1" "owner-1" "name-1"))))
    (should (string= "owner-1/name-1 @host-1"
                     (exordium-forge-cleanup-known-repositories--concat to-delete)))))

(ert-deftest test-exordium-forge-cleanup-known-repositories--concat-2 ()
  (let ((to-delete '(("worktree-1" "host-1" "owner-1" "name-1")
                     ("worktree-2" "host-2" "owner-2" "name-2"))))
    (should (string= (concat "owner-1/name-1 @host-1, "
                             "owner-2/name-2 @host-2")
                     (exordium-forge-cleanup-known-repositories--concat to-delete)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
