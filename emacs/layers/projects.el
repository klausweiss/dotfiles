(use-package projectile
  :config
  (projectile-global-mode)
  )
(use-package helm-projectile
  :after
  (helm projectile)
  :config
  (helm-projectile-on)
  )
