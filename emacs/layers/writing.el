(defun linum-mode-disable ()
  (linum-mode 0))

(defun text-scale-set-big ()
  (text-scale-set 2))

(use-package writeroom-mode
  :init
  (add-hook 'writeroom-mode-hook #'linum-mode-disable)
  (add-hook 'writeroom-mode-hook #'text-scale-set-big)
  :custom
  (word-wrap t)
  (truncate-lines nil)
  )
