(use-package which-key
  :config
  (which-key-mode t)
  )

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
	 )
  )

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
