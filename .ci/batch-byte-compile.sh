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

# Byte compile init.el and all *.el files in modules, themes, and .ci
# directories.  Directory extensions is skipped because this is not Exordium
# code.  Split them into smaller chunks, because Emacs on GitHub worker tends
# to segmentation fault when there's too much to compile in one go.
for pattern in \
        "modules/init-[a-f]*.el" \
        "modules/init-[g-l]*.el" \
        "modules/init-[m-r]*.el" \
        "modules/init-[s-z]*.el" \
        "init.el" \
        "themes/*.el" \
        ".ci/*.el"; do
    echo "===Byte compiling: ${pattern}==="

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
        exordium-spell-check nil
        treesit-auto-install nil)
  (fmakunbound '"'"'ask-user-about-lock)
  (defun ask-user-about-lock (file opponent)
    (sleep-for (+ 1.0 (/ (random 100) 100.0)))
    t)
  (load-file "'"${EMACS_DIR}"'/init.el")
  (setq exordium--require-package-archives package-archives)
  (message "===Byte compilation start: %s==="
           (mapcar (lambda (f)
                     (file-name-nondirectory f))
                   command-line-args-left))
  (batch-byte-compile))' \
            "${files[@]}"
done

cleanup
