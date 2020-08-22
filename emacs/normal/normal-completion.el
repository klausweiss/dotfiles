(require 'normal-autoload)

(autoload-all "../lib/company-box/company-box"
	      #'company-box-mode
	      )

(require 'company)

(with-eval-after-load 'company
  (add-hook 'company-mode-hook #'company-box-mode)
  (setq company-backends '(company-capf
			   company-dabbrev
			   )
	company-idle-delay 0
	company-dabbrev-downcase nil
	tab-always-indent 'complete
	company-quickhelp-delay 0.5

	completion-function #'company-complete-common-or-cycle
	)
  (define-key company-mode-map [escape] #'company-abort)

  (global-company-mode t)
  )

(provide 'normal-completion)
