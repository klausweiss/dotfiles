(setq smart-shift-mode-map 'invalid-keymap)

(with-eval-after-load 'smart-shift
  (setq smart-shift-indentation-level 4)
  (global-smart-shift-mode 1)

  (defun smart-shift-override-local-map ())
  )

(setq expand-region-fast-keys-enabled nil
      expand-region-subword-enabled t)

(provide 'normal-text)
