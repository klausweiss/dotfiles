(require 'normal-autoload)

(autoload-all "lib/normal-window-management"
	      #'get-buffer-window-number
	      )
(autoload-all "../../lib/awesome-tab"
	      #'awesome-tab-kill-all-buffers-in-current-group
	      #'awesome-tab-kill-buffer-match-rule
	      )
(autoload-all "../../lib/projectile"
	      #'projectile-project-name
	      #'projectile-project-root
	      )

(defun normal-close-this-tab-group ()
  (interactive)
  (awesome-tab-kill-all-buffers-in-current-group))

(defun normal-close-this-tab ()
  (interactive)
  (let ((this-tabs-buffers (awesome-tab-tabs (awesome-tab-current-tabset))))
    (if (> (length this-tabs-buffers) 1)
	(kill-this-buffer)
      (if (= 1 (length (get-buffer-window-list)))
	  (kill-buffer-and-window)
	(delete-window))))
  )

(defvar normal-awesome-tab-buffer-group-calc nil)
(defun normal-awesome-tab-buffer-groups ()
  (if normal-awesome-tab-buffer-group-calc
      (symbol-value 'normal-awesome-tab-buffer-group-calc)
    (set (make-local-variable 'normal-awesome-tab-buffer-group-calc)
	 (cond
	  ((string-match "magit-.*-mode" (symbol-name major-mode)) '("Magit"))
          ((condition-case err (projectile-project-root) (error nil))
	   (mapcar (lambda (wn) (concat wn "-" (projectile-project-name))) (get-buffer-windows-numbers)))
	  (t (list (get-buffer-window-number)))
	  ))
    (symbol-value 'normal-awesome-tab-buffer-group-calc)
    ))

(provide 'normal-tabs-lib)
