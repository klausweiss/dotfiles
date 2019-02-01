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
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;; moving between windows
(global-set-key (kbd "C-S-i") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-k") 'windmove-down)
(global-set-key (kbd "C-S-j") 'windmove-left)

;; manipulating windows
(global-set-key (kbd "C-+") 'split-window-below)
(global-set-key (kbd "C-|") 'split-window-right)
(global-set-key (kbd "C-)") 'delete-window)

;; errors
(global-set-key (kbd "<f2>") 'next-error)
(global-set-key (kbd "S-<f2>") 'previous-error)
