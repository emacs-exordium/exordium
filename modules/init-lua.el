;;;; Configuration for Lua
;;;; https://github.com/immerrr/lua-mode

(autoload 'lua-mode "lua-mode" "Major mode for lua files" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . enh-lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . enh-lua-mode))

(provide 'init-lua)
