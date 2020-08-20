(setq magit-file-mode-map 'invalid-keymap)

(with-eval-after-load 'magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (define-key magit-status-mode-map (kbd "M-0") #'magit-section-show-level-1-all)
  )

(provide 'normal-magit)
