;;;; IDO mode for everything (files and buffers)

(require 'ido)
(ido-mode 'both)

;; Display ido results vertically, rather than horizontally (see also
;; ido-vertical-mode):
;;
;; (setq ido-decorations
;;       '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
;;         " [Not readable]" " [Too big]" " [Confirm]"))
;; (defun ido-disable-line-truncation ()
;;   (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;; Open recent files with IDO.
;; `abbreviate-file-name' abbreviates home dir to ~/ in the file list
;; Custom abbreviations can be added to `directory-abbrev-alist'.
(require 'recentf)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun ido-find-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (ido-completing-read "Recentf open: "
                        (mapcar 'abbreviate-file-name recentf-list)
                        nil t)))

(provide 'init-ido)
