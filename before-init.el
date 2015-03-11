(if (string= (getenv "BLOOMBERG_NETWORK") 'corporate)
  ; something for Windows on Bloomberg if true
    (setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "proxy-lon.bloomberg.com:80")
        ("https" . "proxy-lon.bloomberg.com:80")))
)
