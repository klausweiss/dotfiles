(autoload-all "../../lib/perspective"
	      #'persp-scratch-buffer
	      )

(defun normal-close-window-with-buffer ()
  (interactive)
  (if (>= 1 (length (get-buffer-window-list)))
      (kill-buffer-and-window)
    (delete-window)))

(defun normal-new-scratch-buffer ()
  (interactive)
  (let ((new-scratch-buffer (persp-scratch-buffer (persp-current-name))))
    (progn
      (with-current-perspective
	(persp-add-buffer new-scratch-buffer))
      (switch-to-buffer new-scratch-buffer)
      (funcall initial-major-mode)
      ))
  )

(provide 'normal-buffers)
