(require 'normal-themes)

(require 'gruvbox)
(setq dark-theme 'gruvbox-dark-medium)
(setq light-theme 'gruvbox-light-hard)

(require 'dashboard)
(with-eval-after-load 'dashboard
  (dashboard-setup-startup-hook)
  (define-key dashboard-mode-map (kbd "C-p") nil)
  )

(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-dark-theme)

(provide 'normal-interface)

