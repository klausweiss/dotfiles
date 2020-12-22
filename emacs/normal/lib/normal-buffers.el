(require 'normal-autoload)
(require 'seq)

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

(defun normal-can-transparently-kill-buffer (b)
  (or
   (not (buffer-live-p b))
   (string-match "^*scratch*" (buffer-name b))
   )
  )

(defun normal-kill-this-buffer ()
  (interactive)
  (let* ((persp-buffers (persp-current-buffers))
	 (concrete-buffers (seq-remove #'normal-can-transparently-kill-buffer persp-buffers)))
    (if (>= 1 (length concrete-buffers))
	(progn
	  (persp-kill (persp-current-name))
	  (normal-dashboard-open)
	  )
      (kill-this-buffer)
      )
    ))

(provide 'normal-buffers)
