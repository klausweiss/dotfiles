(global-set-key (kbd "C-S-t") 'kill-ring-save)
(global-set-key (kbd "C-t") 'kill-region)

(use-package expand-region
  :bind (("C-'"   . 'er/expand-region)
	 ("C-\"" . 'er/contract-region)
	 )
  )
