(autoload-all "lua-mode"
	      #'lua-mode
	      )
(autoload-all "lsp"
	      #'lsp
	      )
(autoload-all "rustic"
	      #'rustic-mode
	      )
(autoload-all "dockerfile-mode"
	      #'dockerfile-mode
	      )
(autoload-all "cmake-mode"
	      #'cmake-mode
	      )

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-hook 'lua-mode-hook #'lsp)

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

(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(with-eval-after-load 'cmake-mode
  (add-hook 'cmake-mode-hook #'lsp)
  )

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'perl-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(provide 'normal-languages)
