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

(require 'dashboard)
(with-eval-after-load 'dashboard
  (dashboard-setup-startup-hook)
  (define-key dashboard-mode-map (kbd "C-p") nil)
  (setq dashboard-footer-messages
	'("sup"))
  )

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-dark-theme)

(require 'mini-modeline)
(with-eval-after-load 'mini-modeline
  (mini-modeline-mode 1)
  (setq mini-modeline-r-format (list
				"%* "  ;; modified marker
				"%b "  ;; buffer name
				"[%m] "  ;; mode
				"%l (%p)"  ;; line position
				)
	mini-modeline-display-gui-line nil
	mini-modeline-right-padding 1
	mini-modeline-enhance-visual nil
	)

  (set-face-background 'mini-modeline-mode-line "#bdae93")
  )

(provide 'normal-interface)
