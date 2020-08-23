(require 'normal-autoload)

(autoload-all "../../lib/awesome-tab"
	      #'awesome-tab-kill-all-buffers-in-current-group
	      #'awesome-tab-kill-buffer-match-rule
	      )

(defun normal-close-this-tab-group ()
  (interactive)
  (awesome-tab-kill-all-buffers-in-current-group))

(defun normal-close-this-tab ()
  (interactive)
  (let ((this-tabs-buffers (awesome-tab-tabs (awesome-tab-current-tabset))))
    (if (> (length this-tabs-buffers) 1)
	(kill-this-buffer)
      (kill-buffer-and-window)))
    )

(provide 'normal-tabs-lib)