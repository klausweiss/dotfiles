(use-package magit
  :commands magit
  :bind (("C-c g s" . magit))
  :config
  (setq magit-uniquify-buffer-names nil)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )
