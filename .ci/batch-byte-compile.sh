#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${HOME}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

exit_code=0

cleanup () {
    exit_code=$?
    rm -vf "${EMACS_DIR}"/{init.elc,{modules,themes,.ci}/*.elc}
    exit $exit_code
}

trap cleanup ERR INT TERM

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
        user-emacs-directory "'"${EMACS_DIR}"'/"
        exordium-spell-check nil
        treesit-auto-install nil)
  (load-file "'"${EMACS_DIR}"'/init.el")
  (setq exordium--require-package-archives package-archives)
  (message "===Byte compilation start: %s==="
           (mapcar (lambda (f)
                     (file-name-nondirectory f))
                   command-line-args-left))
  (batch-byte-compile))' \
         "${files[@]}"

cleanup
