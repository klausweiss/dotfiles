(require 'windmove)

(defun switch-to-or-split (switch-fun split-fun)
  (condition-case nil
      (call-interactively switch-fun)
    (user-error ; no such window
     (call-interactively split-fun))))

(defun switch-to-or-split-window-right ()
  (interactive)
  (switch-to-or-split #'windmove-right
		      (lambda ()
			(interactive)
			(split-window-horizontally)
			(windmove-right)
			))
  )

(defun switch-to-or-split-window-left ()
  (interactive)
  (switch-to-or-split #'windmove-left #'split-window-horizontally)
  )

(defun switch-to-or-split-window-up ()
  (interactive)
  (switch-to-or-split #'windmove-up #'split-window-vertically)
  )

(defun switch-to-or-split-window-down ()
  (interactive)
  (switch-to-or-split #'windmove-down
		      (lambda ()
			(interactive)
			(split-window-vertically)
			(windmove-down)
			))
  )

(provide 'normal-window-management)
