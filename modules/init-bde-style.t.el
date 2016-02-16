;;; Unit tests for bde-style.el.
;;; To run all tests:
;;;     M-x eval-buffer
;;;     M-x ert

(with-no-warnings (require 'cl))
(require 'init-bde-style)

;; Test apparatus

(cl-defstruct test-case
  input               ;; string: initial buffer content
  output              ;; string: expected final buffer content
  (use-region nil)    ;; select the whole buffer?
  (cursor-pos 0))     ;; move cursor to this position (nil = end of buffer)

(defmacro with-test-case (tst &rest body)
  "Execute BODY using a temporary buffer created according to test case TST.
TST defines the inital content, cursor position, and if a region
is selected or not. Then compare the content of the buffer with
the expected output as defined in TST. Return the final buffer
content string"
  (declare (indent defun))
  `(let ((input      (test-case-input ,tst))
         (output     (test-case-output ,tst))
         (use-region (test-case-use-region ,tst))
         (cursor-pos (test-case-cursor-pos ,tst)))
     (with-temp-buffer
       ;; Set the initial state of the buffer
       (rename-buffer "mqbnet_foo.h")
       (c++-mode)
       (insert input)
       (when cursor-pos (goto-char cursor-pos))
       (when use-region (mark-whole-buffer))
       ;; Execute body
       ,@body
       ;; Return the content of the buffer as raw string (without font-lock)
       (substring-no-properties (buffer-string)))))


;; Tests for `bde-insert-redundant-include-guard-region'

(ert-deftest test-redundant-include-guard-1 ()
  "Breathing test: single #include, no region"
  (let ((tst (make-test-case :input "#include <mqbscm_version.h>
"
                             :output "#ifndef INCLUDED_MQBSCM_VERSION
#include <mqbscm_version.h>
#endif
")))
    (should (string= (with-test-case tst
                       (bde-insert-redundant-include-guard-region))
                   (test-case-output tst)))))

(ert-deftest test-redundant-include-guard-2 ()
  "Test bugfix: output was wrong without a trailing newline"
  (let ((tst (make-test-case :input "#include <mqbscm_version.h>" ; no newline
                             :output "#ifndef INCLUDED_MQBSCM_VERSION
#include <mqbscm_version.h>
#endif
"
                             :cursor-pos nil)))
    (should (string= (with-test-case tst
                       (bde-insert-redundant-include-guard-region))
                     (test-case-output tst)))))



;; Tests for `bde-align-funcall'

(ert-deftest test-bde-align-funcall ()
  (let ((tst (make-test-case :input "
{
    bslma::ManagedPtr<BufferedMessage> message(
          groupInfo->bufferedMessages()[0], &d_bufferedMessagePool);
}
"
                             :output "
{
    bslma::ManagedPtr<BufferedMessage> message(
                                              groupInfo->bufferedMessages()[0],
                                              &d_bufferedMessagePool);
}
"
                             :cursor-pos 62)))
    (should (string= (with-test-case tst
                       (bde-align-funcall))
                     (test-case-output tst)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
