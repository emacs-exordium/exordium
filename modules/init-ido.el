;;; init-ido.el --- IDO mode for everything (files and buffers) -*- lexical-binding: t -*-

;;; Commentary:
;;
;; -------------- -------------------------------------------------------
;; Key            Definition
;; -------------- -------------------------------------------------------
;; C-x C-r        Open recent file with IDO (see also `init-helm.el').


;;; Code:

(use-package ido
  :ensure nil
  :custom
  ;; Ignored files and buffers
  (ido-ignore-files '("\\`#"
                      "\\`.#"
                      "\\`\\.\\./"
                      "\\`\\./"
                      "\\`00"
                      "\\`.*\\.tsk"
                      "\\`ported\\..*"))
  (ido-ignore-buffers '("\\` "
                        "\\*Buffer List\\*"
                        "\\*Help\\*"
                        "\\*Messages\\*"
                        "\\*Completions\\*"))
  (ido-enable-flex-matching t)
  ;; Disabling this, it is buggy
  ;;(ido-use-filename-at-point 'guess)
  :config
  (ido-mode 'both))


;; Open recent files with IDO.
;; `abbreviate-file-name' abbreviates home dir to ~/ in the file list
;; Custom abbreviations can be added to `directory-abbrev-alist'.
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 25)
  :config
  (recentf-mode 1))

(use-package ido-completing-read+
  :defer t)

(defun ido-find-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'."
  (interactive)
  (find-file
   (ido-completing-read "Recentf open: "
                        (mapcar 'abbreviate-file-name recentf-list)
                        nil t)))

(bind-key "C-x C-r" #'ido-find-recentf)



(provide 'init-ido)

;;; init-ido.el ends here
