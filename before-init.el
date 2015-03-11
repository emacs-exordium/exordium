;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Proxies for packages
;;; See DRQS 54913427

;;
;; Bloomberg specific proxy setup, need to comment this out for non BB environment

(if (string= (getenv "ORGANIZATION") 'bloomberg)
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "devproxy.bloomberg.com:82")
        ("https" . "devproxy.bloomberg.com:82")))
)

  ; Windows on Bloomberg if true
(if (string= (getenv "BLOOMBERG_NETWORK") 'corporate)
    (setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "proxy-lon.bloomberg.com:80")
        ("https" . "proxy-lon.bloomberg.com:80")))
)
