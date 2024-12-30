#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${HOME}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

# Use find to find file names such that globs are expanded while prevent
# splitting paths on spaces
mapfile -t files <<< \
        "$(for pattern in "modules/*.el" "init.el" "themes/*.el" ".ci/*.el"; do
               find "${EMACS_DIR}" -type f -path "${EMACS_DIR}/${pattern}"
           done)"
${EMACS} -Q --batch \
         --eval '
(progn
  (setq debug-on-error t
        eval-expression-print-length 100
        edebug-print-length 500
        user-emacs-directory "'"${EMACS_DIR}"'/")
  (load-file "'"${EMACS_DIR}"'/checkdoc-batch/checkdoc-batch.el"))' \
         --funcall checkdoc-batch \
         "${files[@]}"
