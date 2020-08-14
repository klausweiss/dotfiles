(require 'normal-autoload)

(autoload-all "../lib/lsp-mode/lsp"
	      #'lsp
	      )
(autoload-all "../lib/lsp-mode/lsp-modeline"
	      #'lsp-modeline-diagnostics-mode
	      )

(with-eval-after-load 'lsp
  (setq read-process-output-max (* 1024 1024)
	gc-cons-threshold 100000000
	)
  )

(add-hook 'perl-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)

(provide 'normal-lsp)
