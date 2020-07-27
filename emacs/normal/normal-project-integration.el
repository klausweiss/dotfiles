(require 'normal-autoload)

(autoload-all "../lib/magit/magit"
	      #'magit-status
	      )
(autoload-all "../lib/projectile"
	      #'projectile-ensure-project
	      #'projectile-find-file
	      #'projectile-project-root
	      #'projectile-project-p
	      #'projectile-with-default-dir
	      )

(defun projectile-magit-status ()
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (magit-status)))

(defun projectile-find-file-if-in-project ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively #'projectile-find-file)
    (call-interactively #'find-file))
  )


(with-eval-after-load 'projectile
  (setq projectile-switch-project-action #'projectile-magit-status
        projectile-completion-system 'ivy)
  (projectile-mode t)
  )


(with-eval-after-load 'perspective
  (require 'persp-projectile)

  (setq persp-mode-prefix-key (kbd "C-p p"))
  (persp-mode t)
  )

(provide 'normal-project-integration)
