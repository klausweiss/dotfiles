(require 'normal-autoload)

(require 'company)

(with-eval-after-load 'company
  (setq company-backends '(company-capf
			   company-dabbrev
			   )
	company-idle-delay 0
	company-dabbrev-downcase nil
	company-dabbrev-char-regexp "\\sw\\|\\s_"
	tab-always-indent 'complete
	company-quickhelp-delay 0.5
	completion-function #'company-complete-common-or-cycle
	)
  (define-key company-mode-map [escape] #'company-abort)

  (global-company-mode t)
  )

(provide 'normal-completion)
