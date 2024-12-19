#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${HOME}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

for pattern in "modules/*.el" "init.el" "themes/*.el" ".ci/*.el"; do
    echo "===Checkdoc: ${pattern}==="

    # Use find to find file names such that globs are expanded while prevent
    # splitting paths on spaces
    mapfile -t files <<< \
            "$(find "${EMACS_DIR}" -type f -path "${EMACS_DIR}/${pattern}")"

    ${EMACS} -Q --batch \
             --eval '
(progn
  (setq debug-on-error t
        eval-expression-print-length 100
        edebug-print-length 500
        user-emacs-directory "'"${EMACS_DIR}"'/"
        exordium-spell-check nil)
  (load-file "'"${EMACS_DIR}"'/init.el"))' \
             --funcall relint-batch \
             "${files[@]}"
done
