(setq magit-file-mode-map 'invalid-keymap)

(with-eval-after-load 'magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )

(provide 'normal-magit)
