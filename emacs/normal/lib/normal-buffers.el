(autoload-all "../../lib/perspective"
	      #'persp-scratch-buffer
	      )

(defun normal-new-scratch-buffer ()
  (interactive)
  (switch-to-buffer (persp-scratch-buffer (persp-current-name)))
  (funcall initial-major-mode)
  )

(provide 'normal-buffers)
