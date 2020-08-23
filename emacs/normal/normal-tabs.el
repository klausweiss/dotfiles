(require 'awesome-tab)
(require 'normal-autoload)

(autoload-all "lib/normal-window-management"
	      #'get-buffer-window-number
	      )

(with-eval-after-load 'awesome-tab
  (setq awesome-tab-height 120)
  (awesome-tab-mode t)

  (defvar awesome-tab-normal-buffer-group-calc nil)
  (defun awesome-tab-normal-buffer-groups ()
    (if awesome-tab-normal-buffer-group-calc
	(symbol-value 'awesome-tab-normal-buffer-group-calc)
      (set (make-local-variable 'awesome-tab-normal-buffer-group-calc)
	   (cond
	    ((string-match "magit-.*-mode" (symbol-name major-mode)) '("Magit"))
            ((condition-case err (projectile-project-root) (error nil))
	     (list (concat (get-buffer-window-number) "-" (projectile-project-name))))
	    (t (list (get-buffer-window-number)))
	    ))
      (symbol-value 'awesome-tab-normal-buffer-group-calc)
      ))

  (setq awesome-tab-buffer-groups-function #'awesome-tab-normal-buffer-groups)
  )

(provide 'normal-tabs)
