;; themes
(use-package birds-of-paradise-plus-theme)
(use-package gruvbox-theme)
(use-package jbeans-theme
  :pin melpa)
(use-package zerodark-theme)

;; icons
(use-package all-the-icons)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 3)
			  (projects . 5)
			  (agenda   . 5)
			  ))
  :bind (:map dashboard-mode-map
	      ("C-c C-q" . 'dashboard-refresh-buffer)
	      )
  )

;; modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 3)
  (setq doom-modeline-github nil)
  )

;; org
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(load-theme 'zerodark t)

(global-linum-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
