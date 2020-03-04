(use-package gruvbox-theme
  :custom-face
  (treemacs-root-face ((t (:inherit font-lock-string-face :weight bold :height 1.2))))
  (link ((t (:foreground "#fabd2f" :underline t))))
  (widget-button ((t (:inherit link :underline nil :weight bold))))
  :config
  (load-theme 'gruvbox-dark-medium t)
  )
