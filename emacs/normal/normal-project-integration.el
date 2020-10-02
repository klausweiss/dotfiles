(require 'normal-autoload)
;; to have /main/ perspective initialized beefor switching project
(require 'perspective)
(require 'projectile)

(autoload-all "../lib/magit/magit"
	      #'magit-status
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

  (require 'persp-projectile)
  (projectile-persp-bridge projectile-find-file-hook-function)
  )

(with-eval-after-load 'perspective
  (setq persp-mode-prefix-key (kbd "C-p p"))
  (persp-mode t)
  )

(defun persp-counsel-switch-buffer-force ()
  (interactive)
  (require 'counsel)
  (call-interactively #'persp-counsel-switch-buffer)
  )

(provide 'normal-project-integration)
