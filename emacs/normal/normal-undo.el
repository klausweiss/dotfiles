(setq undo-tree-map 'invalid-keymap)

(with-eval-after-load 'undo-tree
  (global-undo-tree-mode)
  )

(provide 'normal-undo)
