(require 'normal-themes)

(require 'gruvbox)
(setq dark-theme 'gruvbox-dark-medium
      light-theme 'gruvbox-light-hard
      font "JetBrains Mono-12:weight=thin"
      )
(with-eval-after-load 'gruvbox
  (set-face-background 'internal-border "#00000000")
  )

(set-face-attribute 'default nil :font font)
(set-frame-font font nil t)

(require 'normal-fonts)
(normal-fonts-enable-ligatures)

(require 'dashboard)
(with-eval-after-load 'dashboard
  (dashboard-setup-startup-hook)
  (define-key dashboard-mode-map (kbd "C-p") nil)
  (setq dashboard-footer-messages
	'("sup"))
  )

(scroll-bar-mode -1)
(tool-bar-mode -1)
(load-dark-theme)

(require 'doom-modeline)
(doom-modeline-mode 1)

(provide 'normal-interface)
