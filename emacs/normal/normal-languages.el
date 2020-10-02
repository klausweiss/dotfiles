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

(add-hook 'perl-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(provide 'normal-languages)
