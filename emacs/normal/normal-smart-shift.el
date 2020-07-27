(setq smart-shift-mode-map 'invalid-keymap)

(with-eval-after-load 'smart-shift
  (setq smart-shift-indentation-level 4)
  (global-smart-shift-mode 1)

  (defun smart-shift-override-local-map ())
  )

(provide 'normal-smart-shift)
