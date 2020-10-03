(defun eval-expression-or-region ()
  (interactive)
  (if (and (use-region-p)
	   (derived-mode-p 'emacs-lisp-mode))
      (call-interactively #'eval-region)
    (call-interactively #'eval-expression)))

(provide 'normal-elisp)
