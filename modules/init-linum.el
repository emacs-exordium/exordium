;;;; Line numbers
;;;
;;; The number one thing that can make Emacs slow is the line number mode.
;;; This module turn it on for all modes except the ones that for which we
;;; don't need it (e.g. using an exception list).
;;;
;;; See also init-ui.el

(require 'linum)
(require 'nlinum)
(require 'init-prefs)

(defun nlinum--setup-window-fudge ()
  "Workaround a bug in older versions of Emacs"
  (let ((width (if (display-graphic-p)
                   (ceiling
                    (let ((width nil))
                      (if width
                          (/ (* nlinum--width 1.0 width)
                             (frame-char-width))
                        (/ (* nlinum--width 1.0
                              16)
                           (frame-char-height)))))
                 nlinum--width)))
    (set-window-margins nil (if nlinum-mode width)
                        (cdr (window-margins)))))

(cond ((and (fboundp 'global-nlinum-mode)
            (eq exordium-display-line-numbers :nlinum))
       ;; Enable nlinum
       (let ((min-version "24.5"))
         (when (version< emacs-version min-version)
           ;; Workaround a bug in Emacs causing emacsclient to display
           ;; "*ERROR*: Invalid face: linum".  See
           ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00079.html
           (fset 'nlinum--setup-window 'nlinum--setup-window-fudge)))

       (global-nlinum-mode t))

      ((and (fboundp 'global-linum-mode)
            exordium-display-line-numbers t)
       (global-linum-mode t)

       (defvar linum-mode-inhibit-modes-list
         '(eshell-mode
           shell-mode
           help-mode
           compilation-mode
           Info-mode
           calendar-mode
           project-explorer-mode
           org-mode
           rtags-rdm-mode
           rtags-diagnostics-mode
           eww-mode)
         "List of modes for which we DO NOT want line numbers")

       (defadvice linum-on (around linum-on-inhibit-for-modes)
         "Stop the load of linum-mode for some major modes."
         (unless (member major-mode linum-mode-inhibit-modes-list)
           ad-do-it))
       (ad-activate 'linum-on)))

(provide 'init-linum)
