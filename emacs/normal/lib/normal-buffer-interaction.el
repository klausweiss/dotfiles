(require 'normal-autoload)

(autoload-all "../lib/smart-shift"
	      #'smart-shift-right
	      )

(defvar completion-function #'completion-at-point)

(defun is-at-word-start ()
  (or
   (not (char-before))
   (memq (get-char-code-property (char-before) 'general-category)
	 '(Zs Cc))
   ))

(defun shift-right/complete/insert-tab ()
  (interactive)
  (cond ((use-region-p) (smart-shift-right))
	((is-at-word-start) (insert-tab))
	(t (call-interactively completion-function))
	)
  )

(provide 'normal-buffer-interaction)
