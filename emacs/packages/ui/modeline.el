;; modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 3)
  (setq doom-modeline-github nil)
  )
