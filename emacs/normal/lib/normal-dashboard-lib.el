(require 'normal-autoload)

(autoload-all "../../lib/emacs-dashboard/dashboard"
	      #'dashboard-insert-startupify-lists
	      )
(autoload-all "../../lib/perspective"
	      #'persp-switch-to-buffer
	      )

(defun normal-dashboard-open ()
  (interactive)
  (persp-switch-to-buffer "*dashboard*")
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  )

(provide 'normal-dashboard-lib)
