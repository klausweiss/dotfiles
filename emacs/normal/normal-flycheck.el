(autoload-all "../lib/flycheck/flycheck"
	      #'flycheck-next-error
	      #'flycheck-first-error
	      )

(defun flycheck-next-or-first-error ()
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (user-error
     (flycheck-first-error))))

(provide 'normal-flycheck)
