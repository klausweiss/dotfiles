(require 'normal-autoload)

(autoload-all "../lib/flycheck/flycheck"
	      #'flycheck-mode
	      )
(autoload-all "../lib/lsp-mode/lsp"
	      #'lsp
	      )
(autoload-all "../lib/lsp-mode/lsp-completion"
	      #'lsp-completion-mode
	      )
(autoload-all "../lib/lsp-mode/lsp-modeline"
	      #'lsp-modeline-diagnostics-mode
	      )
(autoload-all "../lib/yasnippet/yasnippet"
	      #'yas-minor-mode
	      )

(with-eval-after-load 'lsp
  (setq read-process-output-max (* 1024 1024)
	gc-cons-threshold 100000000
	)
  )

(add-hook 'lsp-mode-hook #'flycheck-mode)
(add-hook 'lsp-mode-hook #'lsp-completion-mode)
(add-hook 'lsp-mode-hook #'yas-minor-mode)

(provide 'normal-lsp)
