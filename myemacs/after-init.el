;;
(require 'dired)
(when completion-ignored-extensions
  (add-to-list 'completion-ignored-extensions ".d")
  (add-to-list 'completion-ignored-extensions ".dd")
  (add-to-list 'completion-ignored-extensions ".depends")
  (add-to-list 'completion-ignored-extensions ".linux_64.depends")
  (add-to-list 'completion-ignored-extensions ".t.linux_64.depends")
  (add-to-list 'completion-ignored-extensions ".t.ibm_64.tsk.mapfile")
  (add-to-list 'completion-ignored-extensions ".tsk")
  (add-to-list 'completion-ignored-extensions ".64")
  (add-to-list 'completion-ignored-extensions ".linux_64.d")
  (add-to-list 'completion-ignored-extensions ".linux")
  (add-to-list 'completion-ignored-extensions ".linux_64.c")
  (add-to-list 'completion-ignored-extensions ".pyc"))

;;; IDO ignored extensions
(setq ido-ignore-extensions t)
(setq ido-ignore-buffers
      '("\\` " "^\*Message" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
        "^\*compilation" "^\*GTAGS" "*CEDET" "^\*Python" "^\*Buffer List"
        "^\*Helm" "^\*helm" "^\*magit"))
(load "~/.emacs.d/freds-prefs.el")
