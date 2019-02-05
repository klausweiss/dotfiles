(use-package projectile
  :config
  (projectile-global-mode)
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
