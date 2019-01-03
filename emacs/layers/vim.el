(setq evilkeys (list
		"SPC" 'helm-M-x
		;; buffers
		"bb" 'helm-buffers-list
		"bd" 'kill-this-buffer
		;; files
		"ff"  'helm-find-files
		"fed" 'find-config-file
		"feR" 'reload-config
		;; magit
		"gs" 'magit
		;; projectile
		"pb" 'helm-projectile-switch-to-buffer
		"pf" 'helm-projectile-find-file
		"pp" 'helm-projectile-switch-project
		"pt" 'neotree-project-dir
		))

(use-package evil-leader
  :after
  (evil)
  :config
  (setq evil-leader/leader "SPC")
  (global-evil-leader-mode)
  (apply 'evil-leader/set-key evilkeys))

(use-package evil
  :config
  (evil-mode t)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )

(use-package evil-visualstar
  :after
  (evil)
  :commands (evil-visualstar/begin-search-forward
	     evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))

(use-package evil-magit
  :after
  (evil magit)
  )
