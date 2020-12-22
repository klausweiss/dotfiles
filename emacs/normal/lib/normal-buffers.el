(require 'normal-autoload)

(autoload-all "../../lib/perspective"
	      #'persp-current-buffers
	      )
(autoload-all "../lib/normal-dashboard-lib"
	      #'normal-dashboard-open
	      )

(defun normal-close-window-with-buffer ()
  (interactive)
  (if (>= 1 (length (get-buffer-window-list)))
      (kill-buffer-and-window)
    (delete-window)))

(defun normal-kill-this-buffer ()
  (interactive)
  (let* ((persp-buffers (persp-current-buffers)))
    (if (>= 1 (length persp-buffers))
	(progn
	  (persp-kill (persp-current-name))
	  (normal-dashboard-open)
	  )
      (kill-this-buffer)
      )
    ))

(provide 'normal-buffers)
