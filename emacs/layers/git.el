(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  )
