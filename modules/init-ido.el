;;;; IDO mode for everything (files and buffers)
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-x C-r        Open recent file with IDO or Helm.

(require 'ido)
(ido-mode 'both)


;; Ignored files and buffers

(setq ido-ignore-files '("\\`#"
                         "\\`.#"
                         "\\`\\.\\./"
                         "\\`\\./"
                         "\\`00"
                         "\\`.*\\.tsk"
                         "\\`ported\\..*"))

(setq ido-ignore-buffers '("\\` "
                           "\\*Buffer List\\*"
                           "\\*Help\\*"
                           "\\*Messages\\*"
                           "\\*Completions\\*"))

(setq ido-enable-flex-matching t)

;; Disabling this, it is buggy
;;(setq ido-use-filename-at-point 'guess)


;; Open recent files with IDO.
;; `abbreviate-file-name' abbreviates home dir to ~/ in the file list
;; Custom abbreviations can be added to `directory-abbrev-alist'.
(require 'recentf)
(require 'helm)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun ido-find-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (ido-completing-read "Recentf open: "
                        (mapcar 'abbreviate-file-name recentf-list)
                        nil t)))
(cond (exordium-helm-everywhere
       (define-key global-map [(control x)(control r)] 'helm-recentf))
      (t
       (define-key global-map [(control x)(control r)] 'ido-find-recentf)))


(provide 'init-ido)
