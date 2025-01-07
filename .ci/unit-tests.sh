#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${HOME}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

# Use find to find file names such that globs are expanded while prevent
# splitting paths on spaces
mapfile -t files <<< \
        "$(find "${EMACS_DIR}" -type f -path "${EMACS_DIR}/modules/*.t.el" | \
               grep -v "init-util-cl.t.el")" # this one is covered in unit-test-cl.sh

load_files="
$(printf "  (load-file \"%s\")\n" "${files[@]}")"

# Run all tests form *.t.el
${EMACS} -Q --batch \
         --eval '
(progn
  (setq user-emacs-directory "'"${EMACS_DIR}"'/"
        exordium-spell-check nil)
  (load-file "'"${EMACS_DIR}"'/init.el")'"${load_files}"'
  (let ((print-level 50)
        (eval-expression-print-level 50)
        (eval-expression-print-length 1000)
        (edebug-print-level 50)
        (edebug-print-length 1000)
        (ert-batch-print-level 50)
        (ert-batch-print-length 1000)
        (ert-batch-backtrace-line-length 1000)
        (ert-batch-backtrace-right-margin 1000))
    (ert-run-tests-batch-and-exit)))'
