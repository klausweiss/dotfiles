(require 'normal-autoload)

(autoload-all "../lib/flycheck/flycheck"
	      #'flycheck-mode
	      )
(autoload-all "../lib/lsp-mode/lsp"
	      #'lsp
	      )
(autoload-all "../lib/yasnippet/yasnippet"
	      #'yas-minor-mode
	      )

(with-eval-after-load 'lsp-mode
  (setq read-process-output-max (* 1024 1024)
	gc-cons-threshold 100000000
	lsp-completion-provider :none
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-show-with-cursor nil
	)
  (require 'lsp-completion)
  (require 'lsp-diagnostics)
  (require 'lsp-headerline)
  (require 'lsp-lens)
  (require 'lsp-modeline)
  (require 'lsp-ui)
  )

(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'lsp-mode-hook #'flycheck-mode)
(add-hook 'lsp-mode-hook #'yas-minor-mode)

(provide 'normal-lsp)
