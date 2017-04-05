

;; YCM-gemerator
(defun emacs-ycmd-generate-conf ()
  "Generate `.ycmd_extra_conf.py' for your project.
Ths YCM-Generator is locate in `~/.emacs.d/local/YCM-Generator'."
  (interactive)
  (let* ((cmdfile (concat user-emacs-directory "local/YCM-Generator/config_gen.py"))
         (proot (if (fboundp 'projectile-project-root)
                    (projectile-project-root)
                    (file-name-directory buffer-file-name)))
         (proot-ask
          (read-from-minibuffer "Project Directory: " proot))
         (proc-buff (get-buffer-create"*ycm-generator*")))

    (with-current-buffer proc-buff
      (buffer-disable-undo proc-buff)
      (erase-buffer)

      (let* ((args (apply 'list proot-ask "-v"
                          '()))
             (program+args (append (list cmdfile) args))
             (proc (apply #'start-process "ycm-conf-generator" proc-buff program+args)))

        ;; TODO: error case ?
        )
      )))


(provide 'ycm_generator)
