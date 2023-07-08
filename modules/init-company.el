;;;; company-mode

(use-package company
  :diminish "CA"
  :after (rtags forge)
  :defer

  :init
  (defun exordium-company-assignees (command &optional arg &rest ignored)
    "A `company-mode' backend for assigneees in `forge-mode' repository."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'exordium-company-assignees))
      (prefix
       (save-match-data
         (when (and (or (and (boundp 'git-commit-mode)
                             git-commit-mode)
                        (derived-mode-p 'forge-post-mode))
                    (forge-get-repository 'full)
                    (looking-back
                     (rx "@"
                         (zero-or-one
                          (group alphanumeric
                                 (repeat 0 38
                                         (or alphanumeric
                                             (seq (any "-/") alphanumeric))))))
                     (max (- (point) 40)
                          (point-at-bol))))
           ;; IDK how to match end of a 'symbol' that is equal to an "@" or is
           ;; equal to "@foo" in neither `git-commit-mode' nor
           ;; `forge-post-mode'. Hence it's handled manually.  The
           ;; `looking-back' above matches an "@" or an "@foo". When it was the
           ;; latter there was a match in group 1.  Now, check if this is at
           ;; the very end of the "@" or "@foo".  Note that "@<point>@" also
           ;; matches. Probably a few other characters, substituting the second
           ;; "@" in latter pattern, would also give a positive result. Yet, in
           ;; such a case the `match' is "", so that's all fine - all
           ;; candidates will be shown.
           (when (or (save-match-data (looking-at "\\W"))
                     (= (point) (point-max)))
             (cons (or (match-string 1) "") t)))))
      (candidates (when-let ((repo (forge-get-repository 'full)))
                    (cl-remove-if-not
                     (lambda (assignee)
                       (string-prefix-p arg assignee))
                     (mapcar (lambda (assignee)
                               (propertize (cadr assignee)
                                           'full-name (caddr assignee)))
                             (oref repo assignees)))))
      (annotation (when-let ((assignee (get-text-property 0 'full-name arg)))
                    (format " [%s]" assignee)))))

  :config
  (setq rtags-completions-enabled t)
  ;; Turn on company mode everywhere
  (global-company-mode)
  (add-to-list 'company-backends '(company-capf company-dabbrev))
  (add-to-list 'company-backends 'exordium-company-assignees)
  (setq company-idle-delay nil)

  :bind
  (("C-." . #'company-complete)
   :map company-active-map
        ("<escape>" . #'company-abort)))


(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(provide 'init-company)
