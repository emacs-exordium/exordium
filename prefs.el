;;;; Preferences at Bloomberg

(setq *init-preferred-fonts* nil
      *init-preferred-frame-width* nil
      *init-preferred-frame-height* nil)

(setq default-frame-alist
      (append `(;;(font . ,(choose-frame-font))
                ;;(font . "Monospace 13")
                (font . "LucidaTypewriter 17")
;;                (font . "DejaVu Sans Mono 13")
                ;;(font . "-*-verdana-medium-r-*-*-12-*-*-*-*-*-*-*")
                ;;(font . "-*-consolas-medium-r-*-*-*-*-*-*-*-*-*-*")
                ;;(font . "-*-courier-*-r-*-*-14-*-*-*-*-*-*-*")
                (width . 120)
                (height . 65)
                (vertical-scroll-bars . right)
                (internal-border-width . 0)
                ;;(border-width . 0)
                (horizontal-scroll-bars . t))
              default-frame-alist))

(setq *init-enable-powerline* nil)

(setq *init-rtags-auto-complete* nil)

(setq *init-helm-projectile* t)
(setq *init-enable-electric-pair-mode* nil)
(setq *init-theme* 'tomorrow-night-eighties)
;; (setq *init-line-mode* nil)
