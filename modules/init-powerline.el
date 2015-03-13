;;;; A theme for Powerline

(require 'powerline)
(require 'init-themes)

;;; Fix the grapphical bug with Emacs24.4 on OSX
;;; See https://github.com/milkypostman/powerline/issues/54
(when *environment-osx*
  (setq ns-use-srgb-colorspace nil))

(cond ((featurep 'color-theme-tomorrow)
       (with-tomorrow-colors
        (tomorrow-mode-name)
        ;; These faces are already defined in themes/powerline.el
        (set-face-attribute 'powerline-active1 nil :background selection)
        (set-face-attribute 'powerline-active2 nil :background current-line)
        (set-face-attribute 'powerline-inactive1 nil
                            :background current-line :foreground comment)
        (set-face-attribute 'powerline-inactive2 nil
                            :background background :foreground comment)
        (defface powerline-inactive3
          `((t (:background ,comment :foreground ,background)))
          "Powerline face 3 (buffer name)"
          :group 'powerline)
        (defface powerline-active3
          `((t (:background ,purple :foreground ,background)))
          "Powerline face 3 (buffer name)"
          :group 'powerline)
        (defface powerline-active4
          `((t (:background ,red :foreground ,background)))
          "Powerline face 4 (buffer name when errors)"
          :group 'powerline)
        (defface powerline-active5
          `((t (:background ,green :foreground ,background)))
          "Powerline face 5 (buffer name sans errors)"
          :group 'powerline)))
      ((featurep 'color-theme-monokai)
       (with-monokai-colors
        'default
        ;; These faces are already defined in themes/powerline.el
        (set-face-attribute 'powerline-active1 nil :background monokai-hl)
        (set-face-attribute 'powerline-active2 nil :background monokai-hl-line)
        (set-face-attribute 'powerline-inactive1 nil :background monokai-hl-line)
        (set-face-attribute 'powerline-inactive2 nil :background monokai-bg)
        (defface powerline-inactive3
          `((t (:background ,monokai-comments :foreground ,background)))
          "Powerline face 3 (buffer name)"
          :group 'powerline)
        (defface powerline-active3
          `((t (:background ,violet :foreground ,monokai-bg)))
          "Powerline face 3 (buffer name)"
          :group 'powerline)
        (defface powerline-active4
          `((t (:background ,red :foreground ,monokai-bg)))
          "Powerline face 4 (buffer name when errors)"
          :group 'powerline)
        (defface powerline-active5
          `((t (:background ,green :foreground ,monokai-bg)))
          "Powerline face 5 (buffer name sans errors)"
          :group 'powerline)))
      ((featurep 'color-theme-solarized)
       ;; These faces are already defined in themes/powerline.el
       (set-face-attribute 'powerline-active1 nil
                           :background (second (assoc 'base01 solarized-colors))
                           :foreground (second (assoc 'base2 solarized-colors)))
       (set-face-attribute 'powerline-active2 nil
                           :background (second (assoc 'base02 solarized-colors))
                           :foreground (second (assoc 'base01 solarized-colors)))
       (set-face-attribute 'powerline-inactive1 nil
                           :background (second (assoc 'base02 solarized-colors)))
       (set-face-attribute 'powerline-inactive2 nil
                           :background (second (assoc 'base02 solarized-colors)))
       (defface powerline-inactive3
         `((t (:background ,(second (assoc 'base02 solarized-colors))
               :foreground ,(second (assoc 'base2 solarized-colors)))))
         "Powerline face 3 (buffer name)"
         :group 'powerline)
       (defface powerline-active3
         `((t (:background ,(second (assoc 'base02 solarized-colors))
               :foreground ,(second (assoc 'cyan solarized-colors)))))
         "Powerline face 3 (buffer name)"
         :group 'powerline)
       (defface powerline-active4
         `((t (:background ,(second (assoc 'base02 solarized-colors))
               :foreground ,(second (assoc 'red solarized-colors)))))
         "Powerline face 4 (buffer name when errors)"
         :group 'powerline)
       (defface powerline-active5
         `((t (:background ,(second (assoc 'base02 solarized-colors))
               :foreground ,(second (assoc 'green solarized-colors)))))
         "Powerline face 5 (buffer name sans errors)"
         :group 'powerline)))

(defun my-powerline-theme-buffer-face (active)
  "Return the face to use for the buffer name"
  (cond ((not active)
         ;; Gray
         'powerline-inactive3)
        ((and (eq major-mode 'c++-mode)
              (featurep 'rtags)
              (rtags-has-diagnostics))
         ;; Green or red
         (let ((diag-buff (get-buffer "*RTags Diagnostics*")))
           (if (and diag-buff (> (buffer-size diag-buff) 0))
               'powerline-active4
             'powerline-active5)))
        (t
         ;; Purple
         'powerline-active3)))

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (face3 (if *init-powerline-shows-rtags-diagnostics*
                        (my-powerline-theme-buffer-face active)
                      (if active 'powerline-active3 'powerline-inactive3)))
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
                        ;;(powerline-raw '(:eval (my-test)) face2) ; TODO: test
                        (powerline-vc face2 'r)))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " ")
                        (powerline-raw "%6p" nil 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(my-powerline-theme)

(provide 'init-powerline)
