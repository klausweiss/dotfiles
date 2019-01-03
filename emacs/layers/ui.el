;; themes
(use-package birds-of-paradise-plus-theme)
(use-package gruvbox-theme)
(use-package jbeans-theme)
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
  )

(load-theme 'zerodark t)

(global-linum-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
