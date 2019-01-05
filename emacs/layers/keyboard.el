(use-package which-key
  :config
  (which-key-mode t)
  )

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
	 )
  )

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
