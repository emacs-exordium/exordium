#!/bin/bash

set -x
set -e

EMACS_DIR="$(cd "${GITHUB_WORKSPACE:-~}"/"${1:-.emacs.d}"; pwd -P)/"
EMACS=${EMACS:=emacs}

# Redefine ask-user-about-lock as the melpa seems to stumble on it
# quite often in macos runs. Strategy: wait for 5s then grab the lock
# anyway.
${EMACS} -Q --batch \
         --eval '
(progn
   (setq debug-on-error t
         user-emacs-directory "'"${EMACS_DIR}"'")
   (defun ask-user-about-lock (file opponent)
     (sleep-for 5)
     t)
   (load-file "'"${EMACS_DIR}"'/init.el"))'
