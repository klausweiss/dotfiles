;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 3)
			  (projects . 5)
			  (agenda   . 5)
			  ))
  (add-hook 'kill-buffer-query-functions '(lambda ()
				  (not (member (buffer-name) '("*dashboard*")))))
  :custom-face
  (widget-button ((t (:inherit link :underline nil :weight bold))))
  :bind (:map dashboard-mode-map
	      ("C-c C-q" . 'dashboard-refresh-buffer)
	      )
  )
