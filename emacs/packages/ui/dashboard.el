;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 3)
;			  (projects . 5)
			  (agenda   . 5)
			  ))
  :custom-face
  (widget-button ((t (:inherit link :underline nil :weight bold))))
  :bind (:map dashboard-mode-map
	      ("C-c C-q" . 'dashboard-refresh-buffer)
	      )
  )
