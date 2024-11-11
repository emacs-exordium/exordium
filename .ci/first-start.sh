#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${GITHUB_WORKSPACE:-${HOME}}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

# Redefine ask-user-about-lock as the melpa seems to stumble on it
# quite often in macos runs. Strategy: wait for 5s then grab the lock
# anyway.
${EMACS} -Q --batch \
         --eval '
(progn
  (setq debug-on-error t
        eval-expression-print-length 100
        edebug-print-length 500
        user-emacs-directory "'"${EMACS_DIR}"'/"
        exordium-spell-check nil)
  (fmakunbound '"'"'ask-user-about-lock)
  (defun ask-user-about-lock (file opponent)
    (sleep-for (+ 1.0 (/ (random 100) 100.0)))
    t)
  (load-file "'"${EMACS_DIR}"'/init.el"))'
