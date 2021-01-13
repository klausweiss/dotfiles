(require 'normal-autoload)
(require 'normal-themes)

(autoload-all "lib/normal-buffer-interaction.el"
	      #'normal-linum-format-with-padding
	      )

(require 'gruvbox)
(setq dark-theme 'gruvbox-dark-medium
      light-theme 'gruvbox-light-hard
      font "JetBrains Mono-11:weight=thin"
      )
(with-eval-after-load 'gruvbox
  (set-face-background 'internal-border "#00000000")
  )

(set-face-attribute 'default nil :font font)
(set-frame-font font nil t)

(autoload-all "../lib/page-break-lines"
	      #'page-break-lines-mode
	      )

(require 'dashboard)
(with-eval-after-load 'dashboard
  (dashboard-setup-startup-hook)
  (define-key dashboard-mode-map (kbd "C-p") nil)
  (setq dashboard-footer-messages '("sup")
  	dashboard-projects-switch-function 'projectile-persp-switch-project
	dashboard-items '((projects . 5)
			  (recents  . 5)
			  )
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	)
  )

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-dark-theme)

(show-paren-mode t)

(defun normal-update-linum-format ()
  (setq-local linum-format (normal-linum-format-with-padding)))
(add-hook 'post-command-hook #'normal-update-linum-format)
(add-hook 'prog-mode-hook #'linum-mode)

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
