(autoload-all "../lib/lua-mode"
	      #'lua-mode
	      )
(autoload-all "../lib/lsp-mode/lsp"
	      #'lsp
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

(add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
(add-hook 'rustic-mode-hook #'lsp)
(with-eval-after-load 'rustic
  (with-eval-after-load 'flycheck
    (push 'rustic-clippy flycheck-checkers)
    (setq rustic-flycheck-clippy-params "--message-format=json"
          lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-cargo-watch-enable t
          lsp-rust-analyzer-cargo-all-targets t
          lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-cargo-watch-args "--all-features --tests"
	  )
    ))

(add-to-list 'auto-mode-alist '("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-mode))
(with-eval-after-load 'dockerfile-mode
  (add-hook 'dockerfile-mode-hook #'lsp)
  )

(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.dotfiles/emacs/lib/haskell-mode/")
(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'lsp)
  (setq haskell-hoogle-server-command (lambda (port)
                                        (list "stack" "hoogle" "--" "server"
                                            "--local"
                                            "-p"
                                            (number-to-string port))))
  (setq lsp-haskell-formatting-provider "brittany")
  )

(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(with-eval-after-load 'cmake-mode
  (add-hook 'cmake-mode-hook #'lsp)
  )

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'perl-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(provide 'normal-languages)
