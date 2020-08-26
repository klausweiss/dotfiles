(autoload-all "lua-mode"
	      #'lua-mode
	      )
(autoload-all "lsp"
	      #'lsp
	      )

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-hook 'lua-mode-hook #'lsp)

(add-hook 'perl-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(provide 'normal-languages)
