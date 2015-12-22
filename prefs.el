;;;; Preferences at Bloomberg

(setq default-frame-alist
      (append `(;;(font . ,(choose-frame-font))
                ;;(font . "Monospace 13")
                ;;(font . "LucidaTypewriter 16")
                ;; (font . "DejaVu Sans Mono 13")
                ;;(font . "-*-verdana-medium-r-*-*-12-*-*-*-*-*-*-*")
                ;;(font . "-*-consolas-medium-r-*-*-*-*-*-*-*-*-*-*")
                ;;(font . "-*-courier-*-r-*-*-14-*-*-*-*-*-*-*")
                (width . 120)
                (height . 60)
                (vertical-scroll-bars . right)
                (internal-border-width . 0)
                ;;(border-width . 0)
                (horizontal-scroll-bars . t))
              default-frame-alist))

(setq exordium-helm-projectile t)
(setq exordium-enable-electric-pair-mode nil)
(setq exordium-enable-org-export nil)
(setq exordium-spell-check nil)
(setq exordium-powerline-theme :wave)
;;
;; Now, colorize, accessorize
;;
(setq exordium-theme 'tomorrow-night-awsome)
;(setq exordium-theme 'solarized-dark)
;(setq exordium-theme 'material)
;(setq exordium-theme 'tomorrow-night)
