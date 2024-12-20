;;; init-util.t.el --- Unit tests for init-util.el   -*- lexical-binding: t -*-

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

(use-package el-mock
  :ensure t
  :autoload (mocklet
             mocklet-function))

(require 'ert)
(require 'cl-lib)
(require 'cl-macs)


;; Utility functions

;; The following is useful when need to transfer an expression from, say a
;; `python-mode' to elisp. This has proven useful when creating test cases
;; below, but I don't think it's a general purpose function.

(defun exordium-t-yank-quoted-string ()
  "Yank a string escaping it for emacs-lisp."
  (interactive)
  (insert (cl-prin1-to-string (substring-no-properties (current-kill 0)))))

(cl-defstruct exordium-flip-string-test-case
  "A test case describing a buffer for `exordium-flip-string-quotes'.
The following slots are defined:

- INPUT: the text to be used in a temp buffer before test is run,

- POINT-OR-REGION: the point (when number) or the region (when list) in a
  temp buffer before test is run,

- OUTPUT: a text in a temp buffer after the test has been run."
  (input nil :read-only t)
  (point-or-region nil :read-only t)
  output)

(defmacro with-exordium-flip-string-test-case (test-case &rest body)
  "Execute BODY using a temporary buffer created according to TEST-CASE.
Return the BODY return value"
  (declare (indent defun))
  `(let ((input (exordium-flip-string-test-case-input ,test-case))
         (point-or-region (exordium-flip-string-test-case-point-or-region ,test-case)))
     (with-temp-buffer
       ;; Use `python-mode' for it richness of supported string formats.
       (when (numberp point-or-region)
         (let ((python-mode-hook nil)
               (python-indent-guess-indent-offset-verbose nil))
           (ignore python-mode-hook
                   python-indent-guess-indent-offset-verbose)
           (python-mode)))
       (insert input)
       (cond
        ((numberp point-or-region)
         (goto-char point-or-region))
        ((and point-or-region
              (listp point-or-region))
         (set-mark (car point-or-region))
         (goto-char (cadr point-or-region))
         (activate-mark)))
       (prog1
           ,@body
         (setf (exordium-flip-string-test-case-output ,test-case)
               (substring-no-properties (buffer-string)))))))

;; Tests for `exordium-flip-string--even-chars-between'

(ert-deftest test-exordium-flip-string--even-chars-between-one-dash ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...-.")))
    (should-not (with-exordium-flip-string-test-case test-case
                 (exordium-flip-string--even-chars-between ?- 2 5)))))

(ert-deftest test-exordium-flip-string--even-chars-between-two-dashes ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...--.")))
    (should (with-exordium-flip-string-test-case test-case
             (exordium-flip-string--even-chars-between ?- 2 6)))))

(ert-deftest test-exordium-flip-string--even-chars-between-three-dashes ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...---.")))
    (should-not (with-exordium-flip-string-test-case test-case
                 (exordium-flip-string--even-chars-between ?- 2 7)))))

(ert-deftest test-exordium-flip-string--even-chars-between-four-dashes ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...----.")))
    (should (with-exordium-flip-string-test-case test-case
             (exordium-flip-string--even-chars-between ?- 2 8)))))

(ert-deftest test-exordium-flip-string--even-chars-between-one-dash-begin ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...-.")))
    (should-not (with-exordium-flip-string-test-case test-case
                 (exordium-flip-string--even-chars-between ?- 4 5)))))

(ert-deftest test-exordium-flip-string--even-chars-between-two-dashes-begin ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...--.")))
    (should (with-exordium-flip-string-test-case test-case
             (exordium-flip-string--even-chars-between ?- 4 6)))))

(ert-deftest test-exordium-flip-string--even-chars-between-three-dashes-begin ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...---.")))
    (should-not (with-exordium-flip-string-test-case test-case
                 (exordium-flip-string--even-chars-between ?- 4 7)))))

(ert-deftest test-exordium-flip-string--even-chars-between-four-dashes-begin ()
  (let ((test-case (make-exordium-flip-string-test-case
                    :input "...----.")))
    (should (with-exordium-flip-string-test-case test-case
             (exordium-flip-string--even-chars-between ?- 4 8)))))

;; Tests for `exordium-flip-string-quotes'

(defconst exordium-flip-string--data
  '(("foo01 = \"a string\"" . "foo01 = 'a string'")
    ("foo02 = 'a string'" . "foo02 = \"a string\"")
    ("foo03 = \"\"\"a string\"\"\"" . "foo03 = '''a string'''")
    ("foo04 = '''a string'''" . "foo04 = \"\"\"a string\"\"\"")
    ("foo05 = \"a string 'with quote'\"" . "foo05 = 'a string \\'with quote\\''")
    ("foo06 = 'a string \"with quote\"'" . "foo06 = \"a string \\\"with quote\\\"\"")
    ("foo07 = \"a string \\\\'with quote\\\\'\"" . "foo07 = 'a string \\\\\\'with quote\\\\\\''")
    ("foo08 = \"a string \\'with quote\\'\"" . "foo08 = 'a string \\'with quote\\''")
    ("foo09 = 'a string \\\\\"with quote\\\\\"'" . "foo09 = \"a string \\\\\\\"with quote\\\\\\\"\"")
    ("foo10 = 'a string \\\"with quote\\\"'" . "foo10 = \"a string \\\"with quote\\\"\"")
    ("foo11 = \"\"\"a string 'with quote'\"\"\"" . "foo11 = '''a string 'with quote\\''''")
    ("foo12 = '''a string \"with quote\"'''" . "foo12 = \"\"\"a string \"with quote\\\"\"\"\"")
    ("foo13 = \"\"\"a string \"with quote\\\"\"\"\"" . "foo13 = '''a string \"with quote\"'''")
    ("foo14 = '''a string 'with quote\\''''" . "foo14 = \"\"\"a string 'with quote'\"\"\"")
    ("foo15 = \"\"\"a string \"\"with quote\"\\\"\"\"\"" . "foo15 = '''a string \"\"with quote\"\"'''")
    ("foo16 = '''a string ''with quote'\\''''" . "foo16 = \"\"\"a string ''with quote''\"\"\"")
    ("foo17 = \"\"\"a string 'with quote\\\\'\"\"\"" . "foo17 = '''a string 'with quote\\\\\\''''")
    ("foo18 = '''a string \"with quote\\\\\"'''" . "foo18 = \"\"\"a string \"with quote\\\\\\\"\"\"\"")
    ("foo19 = \"\"\"a string \"with quote\\\\\\\"\"\"\"" . "foo19 = '''a string \"with quote\\\\\"'''")
    ("foo20 = '''a string 'with quote\\\\\\''''" . "foo20 = \"\"\"a string 'with quote\\\\'\"\"\"")
    ("foo21 = \"\"\"a string '''with quote'''\"\"\"" . "foo21 = '''a string \\'\\'\\'with quote\\'\\'\\''''")
    ("foo22 = \"\"\"a string ''with quote''\"\"\"" . "foo22 = '''a string ''with quote'\\''''")
    ("foo23 = '''a string \"\"\"with quote\"\"\"'''" . "foo23 = \"\"\"a string \\\"\\\"\\\"with quote\\\"\\\"\\\"\"\"\"")
    ("foo24 = '''a string \"\"with quote\"\"'''" . "foo24 = \"\"\"a string \"\"with quote\"\\\"\"\"\"")
    ("foo25 = \"\"\"a string \\\"\\\"\\\"with quote\\\"\\\"\\\"\"\"\"" . "foo25 = '''a string \"\"\"with quote\"\"\"'''")
    ("foo26 = '''a string \\'\\'\\'with quote\\'\\'\\''''" . "foo26 = \"\"\"a string '''with quote'''\"\"\"")
    ("foo27 = \"\"\"a string \"with quote\" and something\"\"\"" . "foo27 = '''a string \"with quote\" and something'''")
    ("foo28 = \"\"\"a string \"\"with quote\"\" and something\"\"\"" . "foo28 = '''a string \"\"with quote\"\" and something'''")
    ("foo29 = '''a string 'with quote' and something'''" . "foo29 = \"\"\"a string 'with quote' and something\"\"\"")
    ("foo30 = '''a string ''with quote'' and something'''" . "foo30 = \"\"\"a string ''with quote'' and something\"\"\"")))

(ert-deftest test-exordium-flip-string-quotes-point ()
  (dolist (datum exordium-flip-string--data)
    (let ((test-case (make-exordium-flip-string-test-case
                      :input (car datum)
                      :point-or-region 12))
          (expected (cdr datum)))
      (with-exordium-flip-string-test-case test-case  (exordium-flip-string-quotes))
      (should (string= expected
                       (exordium-flip-string-test-case-output test-case))))))

(ert-deftest test-exordium-flip-string-quotes-region ()
  (dolist (datum exordium-flip-string--data)
    (let ((test-case (make-exordium-flip-string-test-case
                      :input (car datum)
                      :point-or-region `(9 ,(+ 9 (length (car datum))))))
          (expected (cdr datum)))
      (with-exordium-flip-string-test-case test-case  (exordium-flip-string-quotes))
      (should (string= expected
                       (exordium-flip-string-test-case-output test-case))))))

(defconst exordium-flip-string--flip-inner-data
  '(("foo01 = \"a string\"" . "foo01 = 'a string'")
    ("foo02 = 'a string'" . "foo02 = \"a string\"")
    ("foo03 = \"\"\"a string\"\"\"" . "foo03 = '''a string'''")
    ("foo04 = '''a string'''" . "foo04 = \"\"\"a string\"\"\"")
    ("foo05 = \"a string 'with quote'\"" . "foo05 = 'a string \"with quote\"'")
    ("foo06 = 'a string \"with quote\"'" . "foo06 = \"a string 'with quote'\"")
    ("foo07 = \"a string \\\\'with quote\\\\'\"" . "foo07 = 'a string \\\\\"with quote\\\\\"'")
    ("foo08 = \"a string \\'with quote\\'\"" . "foo08 = 'a string \\\"with quote\\\"'")
    ("foo09 = 'a string \\\\\"with quote\\\\\"'" . "foo09 = \"a string \\\\'with quote\\\\'\"")
    ("foo10 = 'a string \\\"with quote\\\"'" . "foo10 = \"a string \\'with quote\\'\"")
    ("foo11 = \"\"\"a string 'with quote'\"\"\"" . "foo11 = '''a string \"with quote\"'''")
    ("foo12 = '''a string \"with quote\"'''" . "foo12 = \"\"\"a string 'with quote'\"\"\"")
    ("foo13 = \"\"\"a string \"with quote\\\"\"\"\"" . "foo13 = '''a string 'with quote\\''''")
    ("foo14 = '''a string 'with quote\\''''" . "foo14 = \"\"\"a string \"with quote\\\"\"\"\"")
    ("foo15 = \"\"\"a string \"\"with quote\"\\\"\"\"\"" . "foo15 = '''a string ''with quote'\\''''")
    ("foo16 = '''a string ''with quote'\\''''" . "foo16 = \"\"\"a string \"\"with quote\"\\\"\"\"\"")
    ("foo17 = \"\"\"a string 'with quote\\\\'\"\"\"" . "foo17 = '''a string \"with quote\\\\\"'''")
    ("foo18 = '''a string \"with quote\\\\\"'''" . "foo18 = \"\"\"a string 'with quote\\\\'\"\"\"")
    ("foo19 = \"\"\"a string \"with quote\\\\\\\"\"\"\"" . "foo19 = '''a string 'with quote\\\\\\''''")
    ("foo20 = '''a string 'with quote\\\\\\''''" . "foo20 = \"\"\"a string \"with quote\\\\\\\"\"\"\"")
    ("foo21 = \"\"\"a string '''with quote'''\"\"\"" . "foo21 = '''a string \"\"\"with quote\"\"\"'''")
    ("foo22 = \"\"\"a string ''with quote''\"\"\"" . "foo22 = '''a string \"\"with quote\"\"'''")
    ("foo23 = '''a string \"\"\"with quote\"\"\"'''" . "foo23 = \"\"\"a string '''with quote'''\"\"\"")
    ("foo24 = '''a string \"\"with quote\"\"'''" . "foo24 = \"\"\"a string ''with quote''\"\"\"")
    ("foo25 = \"\"\"a string \\\"\\\"\\\"with quote\\\"\\\"\\\"\"\"\"" . "foo25 = '''a string \\'\\'\\'with quote\\'\\'\\''''")
    ("foo26 = '''a string \\'\\'\\'with quote\\'\\'\\''''" . "foo26 = \"\"\"a string \\\"\\\"\\\"with quote\\\"\\\"\\\"\"\"\"")
    ("foo27 = \"\"\"a string \"with quote\" and something\"\"\"" . "foo27 = '''a string 'with quote' and something'''")
    ("foo28 = \"\"\"a string \"\"with quote\"\" and something\"\"\"" . "foo28 = '''a string ''with quote'' and something'''")
    ("foo29 = '''a string 'with quote' and something'''" . "foo29 = \"\"\"a string \"with quote\" and something\"\"\"")
    ("foo30 = '''a string ''with quote'' and something'''" . "foo30 = \"\"\"a string \"\"with quote\"\" and something\"\"\"")))

(ert-deftest test-exordium-flip-string-quotes-point-flip-inner ()
  (dolist (datum exordium-flip-string--flip-inner-data)
    (let ((test-case (make-exordium-flip-string-test-case
                      :input (car datum)
                      :point-or-region 12))
          (expected (cdr datum)))
      (with-exordium-flip-string-test-case test-case  (exordium-flip-string-quotes t))
      (should (string= expected
                       (exordium-flip-string-test-case-output test-case))))))

(ert-deftest test-exordium-flip-string-quotes-region-flip-inner ()
  (dolist (datum exordium-flip-string--flip-inner-data)
    (let ((test-case (make-exordium-flip-string-test-case
                      :input (car datum)
                      :point-or-region `(9 ,(+ 9 (length (car datum))))))
          (expected (cdr datum)))
      (with-exordium-flip-string-test-case test-case  (exordium-flip-string-quotes t))
      (should (string= expected
                       (exordium-flip-string-test-case-output test-case))))))


(ert-deftest test-exordium--scratch-kill-buffer-query-function-1 ()
  (let ((buffer (with-current-buffer (scratch)
                  (current-buffer))))
    (unwind-protect
        (progn
          (should buffer)
          (with-current-buffer buffer
            (mocklet ((yes-or-no-p => t :times 1))
              (should (exordium--scratch-kill-buffer-query-function))))
          (with-current-buffer buffer
            (mocklet ((yes-or-no-p => nil :times 1))
              (should-not (exordium--scratch-kill-buffer-query-function)))))
      (when buffer
        (mocklet ((yes-or-no-p => t :times 1))
          (kill-buffer buffer))
        (should-not (buffer-live-p buffer))))))

(ert-deftest test-exordium--scratch-kill-buffer-query-function-2 ()
  (let* ((kill-buffer nil)
         (buffer (or (get-buffer "*scratch*")
                     (prog1
                         (get-buffer-create "*scratch*")
                       (setq kill-buffer t)))))
    (unwind-protect
        (progn
          (should buffer)
          (with-current-buffer buffer
            (mocklet ((yes-or-no-p => t :times 1))
              (should (exordium--scratch-kill-buffer-query-function))))
          (with-current-buffer buffer
            (mocklet ((yes-or-no-p => nil :times 1))
              (should-not (exordium--scratch-kill-buffer-query-function)))))
      (when (and buffer kill-buffer)
        (mocklet ((yes-or-no-p => t :times 1))
          (kill-buffer buffer))
        (should-not (buffer-live-p buffer))))))

(ert-deftest test-exordium--scratch-kill-buffer-query-function-3 ()
  (let ((file (make-temp-file "scratch-")))
    (unwind-protect
        (progn
          (should file)
          (should (file-exists-p file))
          (with-current-buffer (find-file-noselect file)
            (mocklet ((yes-or-no-p not-called))
              (should (exordium--scratch-kill-buffer-query-function)))))
      (when (and file (file-exists-p file))
        (delete-file file)))))



(provide 'init-util.t)

;;; init-util.t.el ends here
