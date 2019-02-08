(use-package projectile
  :config
  (projectile-global-mode)

  (defalias '@project-switch #'helm-projectile-switch-project)
  (defalias '@project-find-file #'helm-projectile-find-file)
  (defalias '@project-switch-buffer #'helm-projectile-switch-to-buffer)
  (defalias '@project-tree #'neotree-project-dir)

  :bind (("C-c p p" . #'@project-switch)
	 ("C-c p f" . #'@project-find-file)
	 ("C-c p b" . #'@project-switch-buffer)
	 ("C-c p t" . #'@project-tree))
  )

(use-package helm-projectile
  :after
  (helm projectile)
  :config
  (helm-projectile-on)
  (define-key helm-projectile-projects-map (kbd "M-g")
    (lambda () (interactive)
      (helm-exit-and-execute-action
       (lambda (dir &rest args)
	 (let ((magit-display-buffer-function
		'magit-display-buffer-fullframe-status-v1))
	   (funcall #'helm-projectile-vc dir))))))
  )
