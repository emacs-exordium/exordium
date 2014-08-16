;;;; Powerline

(require 'powerline)
(require 'color-theme-tomorrow)

(with-tomorrow-colors
 'night
 (defface powerline-active3 `((t (:background ,purple
                                  :foreground ,background)))
  "Powerline face 1."
  :group 'powerline))

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 'powerline-active3)
                          (separator-left (intern
                                           (format "powerline-%s-%s"
                                                   powerline-default-separator
                                                   (car powerline-default-separator-dir))))
                          (separator-right (intern
                                            (format "powerline-%s-%s"
                                                    powerline-default-separator
                                                    (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face3 'l)
                                     (powerline-raw "%b " face3 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (funcall separator-left face3 mode-line)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(my-powerline-theme)

(provide 'init-powerline)
