(autoload-all "../lib/lua-mode"
	      #'lua-mode
	      )
(autoload-all "../lib/rustic/rustic"
	      #'rustic-mode
	      )
(autoload-all "../lib/dockerfile-mode"
	      #'dockerfile-mode
	      )
(autoload-all "../lib/cmake-mode"
	      #'cmake-mode
	      )
(autoload-all "../lib/markdown-mode"
	      #'markdown-mode
	      )
(autoload-all "../lib/yaml-mode"
	      #'yaml-mode
	      )
(autoload-all "../lib/yafolding"
	      #'yafolding-mode
	      )

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(add-hook 'prog-mode-hook #'yafolding-mode)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.dotfiles/emacs/lib/haskell-mode/")

(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(provide 'normal-languages)
