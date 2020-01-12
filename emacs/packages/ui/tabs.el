(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-set-bar 'left)
  :config
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Messages*" name)
       (string-prefix-p "*magit" name)
       )))
  (setq centaur-tabs-style "bar")
  ;;(when (member "Noto Sans" (font-family-list))
  ;;    (centaur-tabs-change-fonts "Noto Sans" 100))
  
  ;; for usage with projectile
  ;; (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  :bind
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  :hook
  (treemacs-mode . centaur-tabs-local-mode)
  )
